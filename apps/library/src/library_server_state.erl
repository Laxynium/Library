-module(library_server_state).

-include("library_server_state.hrl").

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_info/2]).

start_link() ->
   gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init([]) ->
   {ok, #state{clients=[],books=[],version = make_ref()}}.

handle_call(Request,_From,State) ->
   case Request of
      {get_state} -> {reply,{ok,State},State};
      {update_state, NewState} -> 
         if
            State#state.version == NewState#state.version ->
               NewRef = make_ref(),
               {reply, {ok,NewState#state{version = NewRef}},NewState#state{version = NewRef} };
            true ->
               {reply, {failed,State},State}
         end
   end.

handle_cast(_Msg,State) ->
   {noreply,State}.

handle_info(Msg, State) ->
    io:format("Unexpected msg: ~p~n",[Msg]),
    {noreply, State}.

console_log(Message,Data) ->
   gen_server:cast(library_server_man,{log,Message,Data}).

   
    