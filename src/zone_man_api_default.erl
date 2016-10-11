-module(zone_man_api_default).

-export(
   [
    init/3,
    rest_init/2,
    content_types_accepted/2,
    content_types_provided/2,
    forbidden/2,
    resource_exists/2
   ]
  ).

%% cowboy
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, handle_put}], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

forbidden(Req, State) ->
  {false, Req, State}.

resource_exists(Req, State) ->
  case cowboy_req:binding(key, Req) of
    {undefined, Req1} -> {true, Req1, State};
    {Key, Req1} ->
      case application:get_env(example, Key, undefined) of
        undefined -> {false, Req1, State};
        _ -> {true, Req1, State}
      end
  end.
