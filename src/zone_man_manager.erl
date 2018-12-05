-module(zone_man_manager).
-behaviour(gen_statem).
-compile({parse_transform, lager_transform}).

-export([init/1, start_link/1, callback_mode/0]).

-record(state, {spec}).

callback_mode() -> state_functions.

start_link(Spec) ->
  gen_statem:start_link(?MODULE, [Spec], []).

init(Spec) ->
  lager:info("Starting manager ~p", [Spec]),
  {ok, ensure_storage, #state{spec=Spec}}.
