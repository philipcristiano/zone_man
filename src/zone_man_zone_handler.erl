-module(zone_man_zone_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([hello_to_html/2]).
-export([hello_to_json/2]).
-export([hello_to_text/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    io:format("Req: ~p", [Req]),
	{[
		{<<"application/json">>, hello_to_json},
        {'*', hello_to_json}
	], Req, State}.

hello_to_json(Req, State) ->
	Body = <<"{\"rest\": \"Hello World!\"}">>,
	{Body, Req, State}.
