-module(zm_api).
-compile({parse_transform, lager_transform}).

-export([upgrade/4]).


upgrade(Req, Env, Handler, HandlerState) ->
  lager:info("Handler ~p", [Handler]),
  call(Req, Env, Handler, HandlerState).


call(Req, Env, Handler, HandlerState) ->
  lager:info("Env ~p", [Env]),
  {Body, Req2, HS2} = Handler:get(Req, HandlerState),
  Req3 = cowboy_req:set_resp_body(Body, Req2),
  Req4 = cowboy_req:reply(200, Req3),
  Result = cowboy_handler:terminate(normal, Req4, HS2, Handler),
  {ok, Req3, Result}.


