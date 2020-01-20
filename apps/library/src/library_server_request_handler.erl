-module(library_server_request_handler).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-record(state, {}).
start_link() ->
   gen_server:start_link(?MODULE, [], []).

init([]) ->
   {ok, #state{}}.

handle_call(_Request, _From, State) ->
      {reply, not_supported, State}.

handle_cast({ClientPId,UpdateFn,AfterFn},State) ->
   Result = update_library_server(UpdateFn),
   AfterFn(),
   %this is diffrend process then the one Client called, this reply won't be caught by it, chould be cast reply
   gen_server:reply(ClientPId, Result),
   {stop, normal, State}.

handle_info(_Msg, State) ->
   io:format("Unexpected message: ~p~n",[_Msg]),
   {noreply, State}.

update_library_server(UpdateFn) ->
   {ok, State} = gen_server:call(library_server_state,{get_state}),
   {Result,NewState} = UpdateFn(State),
   case Result of 
      {ok,ResultData} ->
         {WriteResult,_ServerState} = gen_server:call(library_server_state,{update_state, NewState}),
         case WriteResult of
            ok -> ResultData;
            failed -> update_library_server(UpdateFn)
         end;
      _ -> Result
   end.



