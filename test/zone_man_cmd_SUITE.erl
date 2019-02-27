-module(zone_man_cmd_SUITE).


-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile({parse_transform, lager_transform}).
-compile(export_all).

-define(MUT, zone_man_netman).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [{create_priv_dir, auto_per_tc}],
             [aa_get_vnic,
              ab_get_vnic_binary,
              ac_get_vnic_not_defined,
              ba_create_vnic,
              ca_get_zone_cfg_no_zone,
              cb_get_zone_cfg_no_zone_binary,
              da_configure_zone
            ]}].


init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    ok = meck:new(zone_man_cmd, [passthrough]),

    Config.

end_per_testcase(_, Config) ->
    ok = meck:unload(zone_man_cmd),
    Config.

aa_get_vnic(_Config) ->
    Name = "dev0",
    meck:expect(zone_man_cmd, run, fun("/usr/sbin/dladm", ["show-vnic", "-p", "-o", "link", "dev0"]) -> {0, Name} end),

    Result = zone_man_cmd:get_vnic(Name),
    ?assertEqual(#{name => Name}, Result),
    ok.

ab_get_vnic_binary(_Config) ->
    Name = <<"dev0">>,
    meck:expect(zone_man_cmd, run, fun("/usr/sbin/dladm", ["show-vnic", "-p", "-o", "link", "dev0"]) -> {0, "dev0"} end),
    Result = zone_man_cmd:get_vnic(Name),

    ?assertEqual(#{name => "dev0"}, Result),
    ok.

ac_get_vnic_not_defined(_Config) ->
    Name = "dev0",
    meck:expect(zone_man_cmd, run, fun(
        "/usr/sbin/dladm", ["show-vnic", "-p", "-o", "link", "dev0"]) ->
            {1, "dladm: invalid vnic name 'dev0': object not found"}
    end),

    Result = zone_man_cmd:get_vnic(Name),
    ?assertEqual(undefined, Result),
    ok.

ba_create_vnic(_Config) ->
    Link = "link0",
    Name = "dev0",
    meck:expect(zone_man_cmd, run, fun("/usr/sbin/dladm", ["create-vnic", "-l", "link0", "dev0"]) -> {0, ok} end),

    Result = zone_man_cmd:create_vnic(Link, Name),
    ?assertEqual(ok, Result),
    ok.

ca_get_zone_cfg_no_zone(_Config) ->
    Name = "dev",

    meck:expect(zone_man_cmd, run, fun("/usr/sbin/zonecfg", ["-z", _, "info"]) -> {1, "dev: No such zone configured"} end),

    Result = zone_man_cmd:get_zone_cfg(Name),
    ?assertEqual(undefined, Result),
    ok.

cb_get_zone_cfg_no_zone_binary(_Config) ->
    Name = <<"dev">>,
    meck:expect(zone_man_cmd, run, fun("/usr/sbin/zonecfg", ["-z", _, "info"]) -> {1, "dev: No such zone configured"} end),
    Result = zone_man_cmd:get_zone_cfg(Name),

    ?assertEqual(undefined, Result),
    ok.

da_configure_zone(_Config) ->
    Name = "dev",
    ZonePath = "/zones/dev",

    meck:expect(zone_man_cmd, run, fun("/usr/sbin/zonecfg", ["-z", _, "create; zonepath=/zones/dev; commit"]) -> {0, ""} end),

    Result = zone_man_cmd:configure_zone(Name, ZonePath, []),
    ?assertEqual(ok, Result),
    ok.

wait_for_message(Msg, Timeout) ->
    ok = receive
      Msg -> ok
    after
      Timeout -> error
    end.

parsed_zone_info() ->
  [{<<"zonename">>, <<"dev">>},
   {<<"zonepath">>, <<"/zones/dev">>},
   {<<"brand">>, <<"ipkg">>},
   {<<"autoboot">>, <<"true">>},
   {<<"ip-type">>, <<"exclusive">>},
   {<<"net">>, [{<<"physical">>, <<"dev0">>}]},
   {<<"net">>, [{<<"physical">>, <<"dev1">>}]}
  ].
