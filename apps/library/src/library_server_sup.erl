-module(library_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    CheckOutPointsSupervisor = #{id => library_checkout_points_sup, start => {library_checkout_points_sup, start_link, []},type=>supervisor},
    Application = #{id => library_server_man, start => {library_server_man, start_link, []}, type=>worker},
    ApplicationState = #{id => library_server_state, start =>{library_server_state, start_link,[]}, type=>worker},
    % Application = #{id => lib_server_core, start => {lib_server_core, start_link, []}, type=>worker},
    StaffPointsSupervisor = #{id => library_staff_points_sup, start => {library_staff_points_sup, start_link, []}, type=>supervisor},
    % BackgroundTimer = #{id => library_server_background_timer, start => {library_server_background_timer, start_link, [{10000, library_server_man, fun () -> calendar:universal_time() end}]}, type=>worker},
    RequestHandlerSupervisor = #{id => library_server_request_handlers_sup, start => {library_server_request_handlers_sup, start_link,[]}, type=>supervisor},
    ChildSpecs = [ApplicationState, Application, RequestHandlerSupervisor, CheckOutPointsSupervisor, StaffPointsSupervisor],
    {ok, {SupFlags, ChildSpecs}}.