-module(zone_man_zone_handler).

-behaviour(zm_api).

-export([init/2]).
-export([zones/2, get/2]).

init(Req, Opts) ->
  {zm_api, Req, Opts}.

zones(Req, State) ->
    {ok, Zones} = zone_man_master:list(),
    Data = [{<<"zones">>, Zones}],
    {Data, Req, State}.

get(Req, State) ->
    zones(Req, State).
