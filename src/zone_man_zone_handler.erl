-module(zone_man_zone_handler).

%% -compile([{parse_transform, lager_transform}]).
-include_lib("mixer/include/mixer.hrl").
-mixin([
        {zone_man_api_default,
         [
          init/3,
          rest_init/2,
          content_types_accepted/2,
          content_types_provided/2,
          resource_exists/2
         ]}
       ]).

-export([allowed_methods/2,
         handle_get/2
        ]).

-behaviour(trails_handler).
-export([trails/0]).


trails() ->
  Metadata =
    #{get =>
      #{tags => ["zone"],
        description => "List zones",
        produces => ["application/json"]
      }
    },
  [trails:trail("/v1/zones", zone_man_zone_handler, [], Metadata)].

%% cowboy
allowed_methods(Req, State) ->
  {[<<"GET">>, <<"HEAD">>], Req, State}.

handle_get(Req, State) ->
    Zones = zone_man_cmd:list_zones(),
    Data = [{<<"zones">>, Zones}],
    Body = jsx:encode(Data),
    {Body, Req, State}.
