-module(zm_api).
-compile({parse_transform, lager_transform}).

-export([upgrade/4]).


-callback get(any(), any()) -> {any(), any(), any()}.

upgrade(Req, Env, Handler, HandlerState) ->
  lager:info("Handler ~p", [Handler]),
  call(Req, Env, Handler, HandlerState).


call(Req, Env, Handler, HandlerState) ->
  lager:info("Env ~p", [Env]),
  Method = cowboy_req:method(Req),
  {Data, Req2, HS2} = call(Method, Req, Env, Handler, HandlerState),
  Body = jsx:encode(Data),
  Req3 = cowboy_req:set_resp_body(Body, Req2),
  Req4 = cowboy_req:reply(200, Req3),
  Result = cowboy_handler:terminate(normal, Req4, HS2, Handler),
  {ok, Req3, Result}.

call(<<"GET">>, Req, _Env, Handler, HandlerState) ->
  Handler:get(Req, HandlerState).
