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

handle_cast({ClientPId, Fn}, State) ->
   Result = Fn(),
   gen_server:reply(ClientPId, Result),
   {noreply, State}.

handle_info(_Msg, State) ->
   io:format("Unexpected message: ~p~n",[_Msg]),
   {noreply, State}.


