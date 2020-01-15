-module(library_application).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link(?MODULE,[]).

init(_Args) ->
    {ok, []}.

handle_call(alloc, _From, _) ->
    {reply, {}, []}.

handle_cast(_, _) ->
    {noreply, {}}.