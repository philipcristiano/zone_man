-module(zone_man_netman_SUITE).


-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile({parse_transform, lager_transform}).
-compile(export_all).

-define(MUT, zone_man_netman).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [{create_priv_dir, auto_per_tc}],
             [aa_ensure_vnic_when_doesnt_exist,
              ab_ensure_vnic_when_exists
            ]}].



init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    ok = meck:new(zone_man_cmd, []),

    Config.

start_mut(NicConfig) ->
    {ok, Pid} = ?MUT:start_link(NicConfig),
    Pid.

end_per_testcase(_, Config) ->
    meck:unload(zone_man_cmd),
    ?MUT:stop(),
    Config.

aa_ensure_vnic_when_doesnt_exist(_Config) ->
    Type = default,
    LinkName = "default0",
    Name = <<"aa_test0">>,
    Opts = [],
    start_mut([{Type, LinkName}]),

    meck:expect(zone_man_cmd, get_vnic, fun(Name) -> undefined end),
    meck:expect(zone_man_cmd, create_vnic, fun(LinkName, Name) -> ok end),

    ok = ?MUT:ensure_vnic(Type, Name, Opts),

    ok.

ab_ensure_vnic_when_exists(_Config) ->
    Type = default,
    LinkName = "default0",
    Name = <<"aa_test0">>,
    Opts = [],
    start_mut([{Type, LinkName}]),

    meck:expect(zone_man_cmd, get_vnic, fun(Name) -> Name end),
    meck:expect(zone_man_cmd, create_vnic, fun(LinkName, Name) -> throw(error) end),

    ok = ?MUT:ensure_vnic(Type, Name, Opts),

    ok.

wait_for_message(Msg, Timeout) ->
    ok = receive
      Msg -> ok
    after
      Timeout -> error
    end.
