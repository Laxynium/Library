-module(library_server_state).

-include("library_server_state.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
   gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init([]) ->
   {ok, #state{clients=[],books=[]}}.

handle_call({get_state}, _From, State)->
    {reply,{ok,State},State}.

handle_cast({update_state, NewState},_State)->
    {noreply, NewState}.
    