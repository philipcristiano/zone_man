-module(zone_man_zone_cfg_SUITE).


-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile({parse_transform, lager_transform}).
-compile(export_all).

-define(MUT, zone_man_zone_cfg).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [{create_priv_dir, auto_per_tc}],
             [aa_ensure_zone_when_doesnt_exist
            ]}].



init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    ok = meck:new(zone_man_cmd, []),
    start_mut(),
    Config.

start_mut() ->
    {ok, Pid} = ?MUT:start_link(),
    Pid.

end_per_testcase(_, Config) ->
    meck:unload(zone_man_cmd),
    ?MUT:stop(),
    Config.

aa_ensure_zone_when_doesnt_exist(_Config) ->
    Name = <<"aa_test">>,
    Spec = #{},

    meck:expect(zone_man_cmd, get_zone_cfg, fun(_Name) -> undefined end),
    meck:expect(zone_man_cmd, configure_zone, fun(_Name, _ZonePath, _Opts) -> ok end),

    ok = ?MUT:configure(Name, Spec).

wait_for_message(Msg, Timeout) ->
    ok = receive
      Msg -> ok
    after
      Timeout -> error
    end.
