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
              ba_create_vnic
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

ba_create_vnic(_Config) ->
    Link = "link0",
    Name = "dev0",
    meck:expect(zone_man_cmd, run, fun("/usr/sbin/dladm", ["create-vnic", "-l", "link0", "dev0"]) -> {0, ok} end),

    Result = zone_man_cmd:create_vnic(Link, Name),
    ?assertEqual(ok, Result),
    ok.

wait_for_message(Msg, Timeout) ->
    ok = receive
      Msg -> ok
    after
      Timeout -> error
    end.
