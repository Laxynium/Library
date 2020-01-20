-module(library_server_man).
-include_lib("core/src/core_book.hrl").
-include_lib("core/src/core_lib_user.hrl").
-include("library_server_state.hrl").
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2 ,handle_info/2]).

-record(man_state, {now}).

start_link() ->
    gen_server:start_link({local, ?MODULE},?MODULE,[],[]).

init(_Args) ->
    {ok, #man_state{now=fun ()->calendar:universal_time() end}}.

handle_call({command, Name, Data}, From, State=#man_state{now=Now}) ->
    case Name of 
        add_book -> execute_command(fun (S) -> add_book(Data,S) end, From, State);
        add_client -> execute_command(fun (S) -> add_client(Data,S, Now) end, From, State);
        borrow_book -> execute_command(fun (S) -> borrow_book(Data,S, Now) end, From, State);
        return_book -> execute_command_with_after(
            fun (S) -> return_book(Data,S, Now) end, From, State, 
            fun () -> gen_server:cast(?MODULE, {update_if_clients_can_borrow})end);
        extend_book -> execute_command(fun (S) -> extend_book(Data,S, Now) end, From, State);
        _ -> {reply, bad_request, State}
    end;

handle_call({query, Name, _Data}, From, State=#man_state{})->
    case Name of
        all_books -> execute_query(fun all_books/1,From, State);
        all_clients -> execute_query(fun all_clients/1,From,State);
        _ -> {reply, bad_request, State}
    end.

execute_command(Fn, From, S)->
    execute_command_with_after(Fn, From,S, fun () -> done end).

execute_command_with_after(Fn, From, S, After)->
    {ok, HandlerPId} = supervisor:start_child(library_server_request_handlers_sup, []),
    gen_server:cast(HandlerPId, {From, Fn, After}),
    {noreply, S}.
        
execute_query(Fn, From, S)->
    {ok, HandlerPId} = supervisor:start_child(library_server_request_handlers_sup, []),
    Action = fun()->
        {ok, State} = gen_server:call(library_server_state, {get_state}),
        {Result} = Fn(State),
        Result
    end,
    gen_server:cast(HandlerPId, {From, Action}),
    {noreply, S}.

%% COMMANDS

%%add actions

add_book({Title, Author, Version}, #state{books=Bs, clients=Cs})->
    {ok,Book} = core_book:create(#book_info{title=Title, author=Author, version=Version}),
    {{ok,Book}, #state{books=[Book|Bs],clients=Cs}}.

add_client({Id, Name}, S = #state{books=Bs, clients=Cs}, Now)->
    IsUnique =  fun (UserId) ->
        not lists:any(fun(#lib_user{id=Id2})-> Id2 == UserId end, Cs)
    end,
    Result = core_lib_user:create(Name, Id, Now, IsUnique),
    case Result of 
        {fail, not_unique} -> {not_unique, S};
        {ok, Client} ->
            {{ok,Client}, #state{books=Bs, clients=[Client|Cs]}}
    end.

%update actions
borrow_book({BookId, ClientId}, S = #state{books=Bs, clients=Cs}, Now) ->
    Book = lists:keyfind(BookId, #book.id, Bs),
    Client = lists:keyfind(ClientId, #lib_user.id, Cs),
    case {Book,Client} of
        {false,_} -> {book_not_found, S};
        {_, false} -> {client_not_found, S};
        _ -> 
            CanBorrow = fun(_C) -> Client#lib_user.can_borrow end,
            Result = core_book:borrow(ClientId, CanBorrow, Now, Book),
            case Result of 
                {fail, Reason} -> {Reason, S};
                {ok, UpdatedBook} -> {{ok,UpdatedBook}, #state{books=lists:keyreplace(BookId, #book.id, Bs, UpdatedBook), clients=Cs}}
            end
    end.

return_book({BookId, ClientId}, S = #state{books=Bs, clients=Cs}, Now)->
    Book = lists:keyfind(BookId, #book.id, Bs),
    Client = lists:keyfind(ClientId, #lib_user.id, Cs),
    case {Book,Client} of
        {false,_} -> 
            {book_not_found, S};
        {_, false} -> 
            {client_not_found, S};
        _ -> 
            Result = core_book:return(ClientId, Now, Book),
            case Result of 
                {fail, Reason} -> 
                    {Reason, S};
                {punishment, Amount, UpdatedBook} -> 
                    {{ok,{punishment, Amount, UpdatedBook}}, #state{books=lists:keyreplace(BookId, #book.id, Bs, UpdatedBook), clients=Cs}};
                {ok, UpdatedBook} -> 
                    {{ok,UpdatedBook}, #state{books=lists:keyreplace(BookId, #book.id, Bs, UpdatedBook), clients=Cs}}
            end
    end.

extend_book({BookId, ClientId}, S = #state{books=Bs, clients=Cs}, Now)->
    Book = lists:keyfind(BookId, #book.id, Bs),
    Client = lists:keyfind(ClientId, #lib_user.id, Cs),
    case {Book,Client} of
        {false,_} -> 
            {book_not_found, S};
        {_, false} -> 
            {client_not_found, S};
        _ -> 
            Result = core_book:extend(ClientId, Now, Book),
            case Result of 
                {fail, Reason} -> 
                    {Reason, S};
                {ok, UpdatedBook} -> 
                    {{ok,UpdatedBook}, #state{books=lists:keyreplace(BookId, #book.id, Bs, UpdatedBook), clients=Cs}}
            end
    end.

%% QUERIES
all_books(#state{books=Bs})->
    {Bs}.

all_clients(#state{clients=Cs}) ->
    {Cs}.

%% Time changing
handle_cast({change_current_date_time, Fn}, _S) ->
    {noreply, #man_state{now=Fn}};

%%Background tasks
handle_cast({update_if_clients_can_borrow}, S=#man_state{})->
    update_clients(S);
handle_cast(_,S)->
    {noreply, S}.


handle_info({update_if_clients_can_borrow}, S=#man_state{})->
    update_clients(S),
    {noreply, S};
handle_info({update_if_clients_can_borrow, update_if_clients_can_borrow_done},S)->
    io:format("Update of clients finished~n"),
    {noreply,S};
handle_info(Msg, State) ->
    io:format("Unexpected msg: ~p~n",[Msg]),
    {noreply, State}.

update_clients(S=#man_state{now=Now}) ->
    Fn = fun (State=#state{clients=Cs,books=Bs}) ->
        OverdueBooks = lists:filter(fun (B) -> core_book:is_overdue(B,Now)end, Bs),
        OverdueClientIds = lists:map(fun (#book{check_out_info=[#check_out_info{by=By}|_]}) -> By end, OverdueBooks),
        OverdueClients = lists:map(
            fun(U=#lib_user{id=Id})->
                IsOverdue = lists:any(fun (Id2)->Id2 == Id end, OverdueClientIds),
                U#lib_user{can_borrow = not IsOverdue}
            end,
            Cs),
        {update_if_clients_can_borrow_done,State#state{clients=OverdueClients}}
    end,
    execute_command(Fn,{self(), update_if_clients_can_borrow},S).