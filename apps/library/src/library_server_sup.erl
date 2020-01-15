-module(library_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    CheckOutPointsSupervisor = #{id => library_checkout_points_sup, start => {library_checkout_points_sup, start_link, []},type=>supervisor},
    Application = #{id => library_application, start => {library_application, start_link, []}, type=>worker},
    StaffPointsSupervisor = #{id => library_staff_points_sup, start => {library_staff_points_sup, start_link, []}, type=>supervisor},
    BackgroundTimer = #{id => library_background_timer, start => {library_background_timer, start_link, [{10000, library_application, fun () -> calendar:universal_time() end}]}, type=>worker},
    ChildSpecs = [Application, CheckOutPointsSupervisor, StaffPointsSupervisor, BackgroundTimer],
    {ok, {SupFlags, ChildSpecs}}.