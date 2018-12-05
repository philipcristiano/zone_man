-module(zone_man_manager_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1,
         start/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start(Spec) ->
  supervisor:start_child(?MODULE, [Spec]).

init([]) ->
  SupFlags = #{strategy => simple_one_for_one,
               intensity => 0,
               period => 1},
  Procs = [
           #{id    => zone_man_master,
             start => {zone_man_manager, start_link, []}
           }],
  {ok, {SupFlags, Procs}}.
