-module(zone_man_cmd_test).

-include_lib("eunit/include/eunit.hrl").

-define(MUT, zone_man_cmd).

parse_machine_zone_test() ->
    MZ = "2:dev:running:/:4872c422-3ad9-4b99-8b9a-a416f282b866:native:excl",
    Zone = ?MUT:parse_machine_zone(MZ),

    Expected = [{id, <<"2">>},
                {name, <<"dev">>},
                {status, <<"running">>},
                {path, <<"/">>},
                {uuid, <<"4872c422-3ad9-4b99-8b9a-a416f282b866">>},
                {brand, <<"native">>},
                {iptype, <<"excl">>}],
    ?assertEqual(Expected, Zone).
