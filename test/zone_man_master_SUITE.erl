-module(zone_man_master_SUITE).


-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile({parse_transform, lager_transform}).
-compile(export_all).

-define(MUT, zone_man_master).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [{create_priv_dir, auto_per_tc}],
             [aa_list_with_no_zones
            ]}].



init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    ok = meck:new(zone_man_cmd, []),
    {ok, Pid} = ?MUT:start_link(),
    [{pid, Pid}|Config].

end_per_testcase(_, Config) ->
    ok = meck:unload(zone_man_cmd),
    Config.

aa_list_with_no_zones(_Config) ->
    meck:expect(zone_man_cmd, list_zones, fun() -> [] end),
    {ok, Zones} = zone_man_master:list(),

    ?assertEqual([], Zones),

    ok.



