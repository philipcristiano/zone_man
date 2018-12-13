-module(zone_man_master_SUITE).


-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile({parse_transform, lager_transform}).
-compile(export_all).

-define(MUT, zone_man_master).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [{create_priv_dir, auto_per_tc}],
             [aa_list_with_no_zones,
              ba_create_zone
            ]}].



init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    ok = meck:new(zone_man_cmd, []),
    ok = meck:new(zone_man_manager_sup, [unstick]),
    Pid = start_mut(Config),
    [{pid, Pid}|Config].

start_mut(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {ok, Pid} = ?MUT:start_link(PrivDir),
    Pid.

end_per_testcase(_, Config) ->
    true = meck:validate(zone_man_cmd),
    true = meck:validate(zone_man_manager_sup),

    ok = meck:unload(zone_man_cmd),
    ok = meck:unload(zone_man_manager_sup),
    ?MUT:stop(),
    Config.

aa_list_with_no_zones(_Config) ->
    meck:expect(zone_man_cmd, list_zones, fun() -> [] end),
    {ok, Zones} = zone_man_master:list(),

    ?assertEqual([], Zones),
    ok.

ba_create_zone(_Config) ->
    meck:expect(zone_man_cmd, list_zones, fun() -> [] end),
    meck:expect(zone_man_manager_sup, start, fun(_) -> {ok, child} end),
    {ok, Zones} = zone_man_master:list(),
    ?assertEqual([], Zones),

    {ok} = zone_man_master:create(<<"test_zone">>),
    {ok, NewZones} = zone_man_master:list(),
    ?assertEqual([<<"test_zone">>], NewZones),

    ok.
