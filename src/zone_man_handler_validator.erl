-module(zone_man_handler_validator).

-export([validate_post/2]).


validate_post(Req, _Schema) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    io:format("~p~n", [Body]),
    {Req2, ok}.

