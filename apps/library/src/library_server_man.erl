-module(library_server_man).
-include_lib("core/src/core_book.hrl").
-include_lib("core/src/core_lib_user.hrl").
-include("library_server_state.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2 ,handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE},?MODULE,[],[]).

init(_Args) ->
    {ok, {fun ()->calendar:universal_time() end}}.

handle_call({command, Name, Data}, From, State={CurrentDateTime}) ->
    case Name of 
        add_book -> execute_command(fun (S) -> add_book(Data,S) end, From, State);
        add_client -> execute_command(fun (S) -> add_client(Data,S, CurrentDateTime) end, From, State);
        borrow_book -> execute_command(fun (S) -> borrow_book(Data,S, CurrentDateTime) end, From, State);
        return_book -> execute_command(fun (S) -> return_book(Data,S, CurrentDateTime) end, From, State);
        extend_book -> execute_command(fun (S) -> extend_book(Data,S, CurrentDateTime) end, From, State);
        _ -> {reply, bad_request, State}
    end;

handle_call({query, Name, _Data}, From, State)->
    case Name of
        all_books -> execute_query(fun all_books/1,From, State);
        all_clients -> execute_query(fun all_clients/1,From,State);
        _ -> {reply, bad_request, State}
    end.

execute_command(Fn, From, S)->
    {ok, HandlerPId} = supervisor:start_child(library_server_request_handlers_sup, []),
    Action = fun() ->
        {ok, State} = gen_server:call(library_server_state, {get_state}),
        {Result,NewState} = Fn(State),
        gen_server:cast(library_server_state, {update_state,NewState}),
        Result
    end,
    gen_server:cast(HandlerPId, {From, Action}),
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
    {Book, #state{books=[Book|Bs],clients=Cs}}.

add_client({Id, Name}, S = #state{books=Bs, clients=Cs}, CurrentDateTime)->
    IsUnique =  fun (UserId) ->
        not lists:any(fun(#lib_user{id=Id2})-> Id2 == UserId end, Cs)
    end,
    Result = core_lib_user:create(Name, Id, CurrentDateTime, IsUnique),
    case Result of 
        {fail, not_unique} -> {not_unique, S};
        {ok, Client} ->
        {Client, #state{books=Bs, clients=[Client|Cs]}}
    end.

%update actions
borrow_book({BookId, ClientId}, S = #state{books=Bs, clients=Cs}, CurrentDateTime) ->
    Book = lists:keyfind(BookId, #book.id, Bs),
    Client = lists:keyfind(ClientId, #lib_user.id, Cs),
    case {Book,Client} of
        {false,_} -> {book_not_found, S};
        {_, false} -> {client_not_found, S};
        _ -> 
            CanBorrow = fun(_C) -> Client#lib_user.can_borrow end,
            Result = core_book:borrow(ClientId, CanBorrow, CurrentDateTime, Book),
            case Result of 
                {fail, Reason} -> {Reason, S};
                {ok, UpdatedBook} -> {UpdatedBook, #state{books=lists:keyreplace(BookId, #book.id, Bs, UpdatedBook), clients=Cs}}
            end
    end.

return_book({BookId, ClientId}, S = #state{books=Bs, clients=Cs}, CurrentDateTime)->
    Book = lists:keyfind(BookId, #book.id, Bs),
    Client = lists:keyfind(ClientId, #lib_user.id, Cs),
    case {Book,Client} of
        {false,_} -> {book_not_found, S};
        {_, false} -> {client_not_found, S};
        _ -> 
            Result = core_book:return(ClientId, CurrentDateTime, Book),
            case Result of 
                {fail, Reason} -> {Reason, S};
                {ok, UpdatedBook} -> {UpdatedBook, #state{books=lists:keyreplace(BookId, #book.id, Bs, UpdatedBook), clients=Cs}}
            end
    end.

extend_book({BookId, ClientId}, S = #state{books=Bs, clients=Cs}, CurrentDateTime)->
    Book = lists:keyfind(BookId, #book.id, Bs),
    Client = lists:keyfind(ClientId, #lib_user.id, Cs),
    case {Book,Client} of
        {false,_} -> {book_not_found, S};
        {_, false} -> {client_not_found, S};
        _ -> 
            Result = core_book:extend_book(ClientId, CurrentDateTime, Book),
            case Result of 
                {fail, Reason} -> {Reason, S};
                {ok, UpdatedBook} -> {UpdatedBook, #state{books=lists:keyreplace(BookId, #book.id, Bs, UpdatedBook), clients=Cs}}
            end
    end.

%% QUERIES
all_books(#state{books=Bs})->
    {Bs}.

all_clients(#state{clients=Cs}) ->
    {Cs}.

%%UNUSED
handle_cast({change_current_date_time, Fn}, _S) ->
    {noreply, {Fn}};
handle_cast(_,S)->
    {noreply, S}.

handle_info(_Msg, State) ->
    {noreply, State}.