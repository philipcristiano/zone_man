-module(zone_man_app).
-behaviour(application).
-compile([{parse_transform, lager_transform}]).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  check_config(),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/v1/zones", zone_man_zone_handler, []}
    ]}
  ]),

  CertsDir = case application:get_env(zone_man,
                                      certificates_directory,
                                      undefined) of
      undefined -> "certs";
      Val -> Val
  end,
  CACertFile = CertsDir ++ "/ca.pem",
  CertFile = CertsDir ++ "/server.pem",
  KeyFile = CertsDir ++ "/server-key.pem",

  lager:info("Checking for file ~p", [CACertFile]),
  true = filelib:is_regular(CACertFile),
  lager:info("Checking for file ~p", [CertFile]),
  true = filelib:is_regular(CertFile),
  lager:info("Checking for file ~p", [KeyFile]),
  true = filelib:is_regular(KeyFile),

  cowboy:start_tls(https, [{port, 8443},
                           {cacertfile, CACertFile},
                           {certfile, CertFile},
                           {keyfile, KeyFile},
                           {fail_if_no_peer_cert, true},
                           {server_name_indication, disable},
                           {verify, verify_peer}],
                  #{env => #{dispatch => Dispatch}
  }),

  zone_man_sup:start_link().

stop(_State) ->
  ok.

check_config() ->
  zone_man_netman:check_config().
