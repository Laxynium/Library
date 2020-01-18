-module(library_server_request_handlers_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args)->
    SupFlags=#{strategy => simple_one_for_one},
    ChildSpecs = [#{id => library_server_request_handler, start => {library_server_request_handler, start_link, []}, restart=>temporary, shutdown=>brutal_kill}],
    {ok, {SupFlags, ChildSpecs}}.

    