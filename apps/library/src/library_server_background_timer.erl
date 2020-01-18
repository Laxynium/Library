-module(library_server_background_timer).

-include_lib("core/src/core_book.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, handle_info/2, handle_cast/2, handle_call/3]).

start_link({Period, DataProcessName, Now}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Period, DataProcessName, Now}, []).

init({Period, DataProcessName, Now}) ->
    Timer = erlang:send_after(1, self(), update),
    {ok, {Period, DataProcessName, Now, Timer}}.

handle_cast(_,_)->ok.
handle_call(_,_,_)->{reply,{}}.

handle_info(update, {Period, DataProcessName, Now, OldTimer}) ->
    erlang:cancel_timer(OldTimer),
    DataProcessId = erlang:whereis(DataProcessName),
    DataProcessId ! {update_if_client_can_rent, self(), get, all_books},

    Timer = erlang:send_after(Period, self(), update),
    {noreply, {Period, DataProcessName, Now, Timer}};

handle_info({update_if_client_can_rent, result, AllBooks}, {Period, DataProcessName, Now, OldTimer}) ->
    OverdueBooks = lists:filter(fun (B) -> core_book:is_overdue(B, Now) end, AllBooks),
    OverdueStudents = lists:map(fun ({_,_,[{_,_,By,_,_}|_]})-> By end, OverdueBooks),
    DataProcessId = erlang:whereis(DataProcessName),
    DataProcessId ! {update_if_client_can_rent, result, OverdueStudents},
    {noreply, {Period, DataProcessName, Now, OldTimer}}.