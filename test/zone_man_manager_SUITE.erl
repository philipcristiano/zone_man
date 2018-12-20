-module(zone_man_manager_SUITE).


-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile({parse_transform, lager_transform}).
-compile(export_all).

-define(MUT, zone_man_manager).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [{create_priv_dir, auto_per_tc}],
             [aa_simple_create
            ]}].



init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    Config.

start_mut(Spec) ->
    {ok, Pid} = ?MUT:start_link(Spec),
    Pid.

end_per_testcase(_, Config) ->
    true = meck:validate(zone_man_cmd),
    true = meck:validate(zone_man_manager_sup),

    ok = meck:unload(zone_man_cmd),
    ok = meck:unload(zone_man_manager_sup),
    ?MUT:stop(),
    Config.

aa_simple_create(_Config) ->
    ok.

wait_for_message(Msg, Timeout) ->
    ok = receive
      Msg -> ok
    after
      Timeout -> error
    end.
