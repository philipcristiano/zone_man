-module(zone_man_netman_SUITE).


-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile({parse_transform, lager_transform}).
-compile(export_all).

-define(MUT, zone_man_netman).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [{create_priv_dir, auto_per_tc}],
             [aa_simple_ensure_vnic
            ]}].



init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    ok = meck:new(zone_man_cmd, []),

    Config.

start_mut() ->
    {ok, Pid} = ?MUT:start_link(),
    Pid.

end_per_testcase(_, Config) ->
    meck:expect(zone_man_cmd, list_zones, fun() -> [] end),
    ?MUT:stop(),
    Config.

aa_simple_ensure_vnic(_Config) ->
    ok.

wait_for_message(Msg, Timeout) ->
    ok = receive
      Msg -> ok
    after
      Timeout -> error
    end.
