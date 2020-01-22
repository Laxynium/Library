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
    global:register_name(?MODULE,self()),
    {ok, #man_state{now=fun ()->calendar:universal_time() end}}.

handle_call(_Data,From,State) ->
    {reply,not_supported,State}.

%CAST HANDLING

%update handling
handle_cast({command, CommandType, From, Data}, State=#man_state{now=Now}) ->
    case CommandType of 
        add_book -> execute_command(From,fun (S) -> add_book(Data,S) end, State);
        add_client -> execute_command(From,fun (S) -> add_client(Data,S, Now) end, State);
        borrow_book -> execute_command(From,fun (S) -> borrow_book(Data,S, Now) end, State);
        return_book -> execute_command_with_after( From,
            fun (S) -> return_book(Data,S, Now) end,  
            fun () -> gen_server:cast(?MODULE, {update_if_clients_can_borrow})end,
            State);
        extend_book -> execute_command(From, fun (S) -> extend_book(Data,S, Now) end, State);
        _ -> {reply, bad_request, State}
    end;
%query handling
handle_cast({query, QueryType, From, Data},State=#man_state{})->
    case QueryType of
        all_books -> execute_query(From,fun all_books/1, State);
        all_clients -> execute_query(From,fun all_clients/1,State);
        _ -> {reply, bad_request, State}
    end;


%console logging
handle_cast({log,Message,Data},S) ->
    io:format(Message,Data),
    {noreply, S};

handle_cast({echo,From},S) ->
    gen_server:cast(From,echo),
    {noreply, S};

%% Time changing
handle_cast({change_current_date_time, Fn}, _S) ->
    {noreply, #man_state{now=Fn}};

handle_cast(_,S)->
    {noreply, S}.

execute_command(From,Fn, S) ->
    execute_command_with_after(From, Fn, fun () -> ok end,S).

execute_command_with_after(From, Fn,After, S)->
    {ok, HandlerPId} = supervisor:start_child(library_server_request_handlers_sup, []),
    gen_server:cast(HandlerPId, {update,{From, Fn, After}}),
    {noreply, S}.
        
execute_query(From,Fn, S)->
    execute_query_with_after(From,Fn,fun() -> ok end,S).
execute_query_with_after(From,Fn,After, S)->
    {ok, HandlerPId} = supervisor:start_child(library_server_request_handlers_sup, []),
    gen_server:cast(HandlerPId, {query,{From, Fn,fun () -> ok end}}),
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
            {{fail,book_not_found}, S};
        {_, false} -> 
            {{fail,client_not_found}, S};
        _ -> 
            Result = core_book:return(ClientId, Now, Book),
            case Result of 
                {fail, Reason} -> 
                    {{fail,Reason}, S};
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
            {{fail,book_not_found}, S};
        {_, false} -> 
            {{fail,client_not_found}, S};
        _ -> 
            Result = core_book:extend(ClientId, Now, Book),
            case Result of 
                {fail, Reason} -> 
                    {{fail,Reason}, S};
                {ok, UpdatedBook} -> 
                    {{ok,UpdatedBook}, #state{books=lists:keyreplace(BookId, #book.id, Bs, UpdatedBook), clients=Cs}}
            end
    end.

%% QUERIES
all_books(#state{books=Bs})->
    {Bs}.

all_clients(#state{clients=Cs}) ->
    {Cs}.

%handle update signal
handle_info({update_if_clients_can_borrow}, S=#man_state{})->
    io:format("Received signal to refresh client data~n"),
    update_clients(S),
    {noreply, S};

handle_info({ok, update_if_clients_can_borrow_done},S)->
    %io:format("Update of clients finished~n"),
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
        {{ok,update_if_clients_can_borrow_done},State#state{clients=OverdueClients}}
    end,
    After = fun() -> library_server_request_handler:console_log("Update of clients finished~n",[]) end,
    execute_command_with_after(self(),Fn,After,S).