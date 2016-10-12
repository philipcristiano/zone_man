-module(zone_man_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Port = 8080,
    ListenerCount = 100,
    Trails = trails:trails([zone_man_zone_handler,
                            cowboy_swagger_handler]),
    trails:store(Trails),
    Dispatch = trails:single_host_compile(Trails),
    RanchOptions = [{port, Port}],
    CowboyOptions =
      [
       {env,
        [
         {dispatch, Dispatch}
        ]},
       {compress, true},
       {timeout, 12000}
    ],
    {ok, _} =
        cowboy:start_http(example_http, ListenerCount, RanchOptions, CowboyOptions),
    %     cowboy:start_clear(http, 100, [{port, 8080}], #{
    %         env => #{dispatch => Dispatch}
    %     }),

	zone_man_sup:start_link().

stop(_State) ->
	ok.
