-module(zone_man_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  FileBase = application:get_env(zone_man, root, "tmp"),
  Procs = [#{id    => zone_man_manager_sup,
             start => {zone_man_manager_sup, start_link, []},
             type  => supervisor
           },
           #{id    => zone_man_master,
             start => {zone_man_master, start_link, [FileBase]}
           }],
  {ok, {{one_for_one, 1, 5}, Procs}}.
