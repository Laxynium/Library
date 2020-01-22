-module(library_server_request_handler).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,console_log/2]).
-record(state, {}).
start_link() ->
   gen_server:start_link(?MODULE, [], []).

init([]) ->
   {ok, #state{}}.

handle_call(_Request, _From, State) ->
      {reply, not_supported, State}.
handle_cast({OpType,Data},State) ->
   case OpType of
      update -> handle_update_job(Data);
      query -> handle_query_job(Data);
      arbitrary -> handle_arbitrary_job(Data);
      _ -> console_log("handler recieved unknown operation type of: ~p~n",[OpType])
   end,
   {stop, normal, State}.

handle_update_job({ClientPId,UpdateFn,AfterFn}) ->
   JobResult = update_library_server(UpdateFn),
   AfterFn(),
   console_log("update requested by ~p finished with result ~p~n",[ClientPId,JobResult]),
   gen_server:cast(ClientPId,JobResult).

handle_query_job({ClientPId,QueryFn,AfterFn}) ->
   {ok, DatabaseState} = gen_server:call(library_server_state,{get_state}),
   JobResult = QueryFn(DatabaseState),
   AfterFn(),
   console_log("request by ~p finished successfully with result ~p~n",[ClientPId,JobResult]),
   gen_server:cast(ClientPId,JobResult).

handle_arbitrary_job({ClientPId,ArbitraryFn,AfterFn}) ->
   Result = ArbitraryFn(),
   AfterFn,
   gen_server:cast(ClientPId,Result).

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
            ok -> 
               Result;
            failed -> 
               update_library_server(UpdateFn)
         end;
      _ -> Result
   end.

console_log(Message,Data) ->
   gen_server:cast(library_server_man,{log,Message,Data}).



