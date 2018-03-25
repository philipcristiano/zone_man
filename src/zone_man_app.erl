-module(zone_man_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/v1/zones", zone_man_zone_handler, []}
    ]}
  ]),
  cowboy:start_clear(http, [{port, 8080}], #{
    env => #{dispatch => Dispatch}
  }),
  cowboy:start_tls(https, [{port, 8443},
                           {cacertfile, "certs/ca.pem"},
		                       {certfile, "certs/server.pem"},
                           {keyfile, "certs/server-key.pem"}],
                  #{env => #{dispatch => Dispatch}
  }),

  zone_man_sup:start_link().

stop(_State) ->
  ok.
