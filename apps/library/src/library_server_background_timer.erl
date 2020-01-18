-module(library_server_background_timer).

-include_lib("core/src/core_book.hrl").
-include("library_server_background_timer.hrl").
-behaviour(gen_server).

-export([start_link/1, init/1, handle_info/2, handle_cast/2, handle_call/3]).

-record(timer_state, {subscribers}).

start_link(Subscribers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #timer_state{subscribers=Subscribers}, []).

init(#timer_state{subscribers=Ss}) ->
    Do = fun (S=#subscriber{}) -> 
        Timer = erlang:send_after(1, self(), {update, S}),
        S#subscriber{timer=Timer}
    end,
    NewSubs = lists:map(Do,Ss),
    {ok, #timer_state{subscribers=NewSubs}}.

handle_cast(_,_)->ok.
handle_call(_,_,_)->{reply,{}}.

handle_info({update, S=#subscriber{timer=Timer, process_name=ProcessName, message=Message, period=Period}}, #timer_state{subscribers=Ss})->
    if Timer /= undefined -> 
        erlang:cancel_timer(Timer);
        true -> ok
    end,
    ProcessId = erlang:whereis(ProcessName),
    ProcessId ! Message,
    NewTimer = erlang:send_after(Period, self(), {update,S}),
    NS = S#subscriber{timer=NewTimer},
    UpdatedSubs = lists:keyreplace(ProcessName,#subscriber.process_name, Ss, NS),
    {noreply, #timer_state{subscribers=UpdatedSubs}}.