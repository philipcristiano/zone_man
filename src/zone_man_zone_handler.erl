-module(zone_man_zone_handler).
-behaviour(trails_handler).

%% -compile([{parse_transform, lager_transform}]).

-export([allowed_methods/2,
         handle_get/2,
         handle_post/2,
         init/3,
         rest_init/2,
         content_types_accepted/2,
         content_types_provided/2
        ]).

-export([trails/0]).

%% cowboy
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

content_types_accepted(Req, State) ->
    io:format("state ~p~n", [State]),
    {[{<<"application/json">>, handle_post}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

trails() ->
    Schema = zone_schema(),
    RequestBody = #{
        name => <<"request body">>,
        in => body,
        description => <<"request body (as json)">>,
        required => true,
        schema => Schema
    },
    Metadata = #{
        get => #{
            tags => ["zone"],
            description => "List zones",
            produces => ["application/json"]
      },
        post => #{
            tags => ["zone"],
            description => "Create a zone",
            produces => ["application/json"],
            parameters => [RequestBody]
        }
      },
    [trails:trail("/v1/zones", zone_man_zone_handler, [], Metadata)].

%% cowboy
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"HEAD">>], Req, State}.

handle_get(Req, State) ->
    Zones = <<"zone">>,%  zone_man_cmd:list_zones(),
    Data = [{<<"zones">>, Zones}],
    Body = jsx:encode(Data),
    {Body, Req, State}.

handle_post(Req, State) ->
    validate_or_fail(Req, State, fun do_post/2, zone_schema()).
    {Req2, ParsedBody} = zone_man_handler_validator:validate_post(Req, zone_schema()),
    io:format("~p~n", [ParsedBody]),
    Zones = <<"posted zone">>, %zone_man_cmd:list_zones(),
    Data = [{<<"zones">>, Zones}],
    RespBody = jsx:encode(Data),
    Req3 = cowboy_req:set_resp_body(RespBody, Req2),
    {true, Req3, State}.

validate_or_fail(Req, State, Handler, Schema) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Data = jsx:decode(Body),

    {true, Req3, State}.


zone_schema() ->
    #{
        <<"name">> =>
            #{type => <<"string">>,
              description => <<"Zone Name">>
            }
    }.
