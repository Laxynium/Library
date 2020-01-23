-module(library_server_sup).
-include("library_server_background_timer.hrl").
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    
    Application = #{id => library_server_man, start => {library_server_man, start_link, []}, type=>worker},
    ApplicationState = #{id => library_server_state, start =>{library_server_state, start_link,[]}, type=>worker},
    BackgroundTimer = #{
        id => library_server_background_timer, 
        start => {
            library_server_background_timer, 
            start_link, 
            [[#subscriber{process_name=library_server_man, period=100000,message={update_if_clients_can_borrow}}]]
        }, 
        type=>worker
    },
    RequestHandlerSupervisor = #{id => library_server_request_handlers_sup, start => {library_server_request_handlers_sup, start_link,[]}, type=>supervisor},
    ChildSpecs = [ApplicationState, Application, RequestHandlerSupervisor, BackgroundTimer],
    {ok, {SupFlags, ChildSpecs}}.