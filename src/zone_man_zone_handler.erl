-module(zone_man_zone_handler).
-compile({parse_transform, lager_transform}).

-behaviour(zm_api).

-export([init/2]).
-export([zones/2, get/2]).
-export([post/3, post_spec/0]).

init(Req, Opts) ->
  {zm_api, Req, Opts}.

zones(Req, State) ->
    {ok, Zones} = zone_man_master:list(),
    Data = [{<<"zones">>, Zones}],
    {Data, Req, State}.

get(Req, State) ->
    zones(Req, State).

post_spec() ->
    #{<<"name">> => #{type => string}
    }.

post(Req, State, Data) ->
    lager:info("Data ~p", [Data]),
    Name = maps:get(<<"name">>, Data),
    zone_man_master:create(Name),
    {Data, Req, State}.
