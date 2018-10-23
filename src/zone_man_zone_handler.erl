-module(zone_man_zone_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([zones/2]).

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, zones},
        {'*', hello_to_json}
  ], Req, State}.

zones(Req, State) ->
    {ok, Zones} = zone_man_master:list(),
    Data = [{<<"zones">>, Zones}],
    Body = jsx:encode(Data),
    {Body, Req, State}.
