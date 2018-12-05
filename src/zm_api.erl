-module(zm_api).
-compile({parse_transform, lager_transform}).

-export([upgrade/4, validate_spec_to_data/2]).


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
  Handler:get(Req, HandlerState);
call(<<"POST">>, Req, _Env, Handler, HandlerState) ->
  Spec = Handler:post_spec(),
  {Data, _Errors} = spec_to_data(Spec, Req),
  Handler:post(Req, HandlerState, Data).

spec_to_data(Spec, Req) ->
  Bin = read_body(Req),
  Data = jsx:decode(Bin, [return_maps]),
  validate_spec_to_data(Spec, Data).

validate_spec_to_data(Spec, Data) ->
  validate_spec_to_data(Spec, Data, []).

validate_spec_to_data(Spec, Data, _Errors) when is_map(Spec) ->
  SpecL = maps:to_list(Spec),
  validate_spec_to_data(SpecL, Data, #{});
validate_spec_to_data(Spec=[], _Data, _Errors) when is_list(Spec) ->
  {#{}, []};
validate_spec_to_data(
    Spec=[{H, SingleSpec}| T], Data, Errors) when is_list(Spec) ->
  lager:info("validating ~p ~p", [H, SingleSpec]),
  {OurData, OurErrors} = case maps:is_key(H, Data) of
    true -> Val = maps:get(H, Data),
            {#{H => Val}, []};
    false ->
            {#{}, [{field_missing, H}]}
  end,

  {NextData, NextErrors} = validate_spec_to_data(T, Data, Errors),
  ReturnData = maps:merge(OurData, NextData),
  ReturnErrors = OurErrors ++ NextErrors,
  {ReturnData, ReturnErrors}.


read_body(Req) ->
  read_body(more, Req).

read_body(ok, _Req) ->
  <<>>;
read_body(more, Req) ->
  {Status, OurBin, Req1} = cowboy_req:read_body(Req),
  lager:info(OurBin),
  MoreBin = read_body(Status, Req1),
  <<OurBin/binary, MoreBin/binary>>.

