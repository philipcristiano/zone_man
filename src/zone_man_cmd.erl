-module(zone_man_cmd).

-export([run/2,
         list_zones/0,
         parse_machine_zone/1]).

list_zones() ->
    MachineZones = zone_man_cmd:run("/usr/sbin/zoneadm", ["list", "-p"]),
    Zones = parse_machine_zones(MachineZones),
    Zones.

parse_machine_zones(MachineZones) ->
    parse_machine_zones(MachineZones, []).

parse_machine_zones([], Zones) ->
    Zones;
parse_machine_zones([H|T], Zones) ->
    Zone = parse_machine_zone(H),
    parse_machine_zones(T, [Zone|Zones]).

parse_machine_zone(MZ) ->
    [ID, Name, Status, Path, UUID, Brand, IPType] = re:split(MZ, ":"),
    [{id, ID},
     {name, Name},
     {status, Status},
     {path, Path},
     {uuid, UUID},
     {brand, Brand},
     {iptype, IPType}].


run(CMD, Options) ->
    Port = open_port({spawn_executable, CMD}, [{line, 4096},
                                               eof,
                                               {args, Options}]),
    do_read(Port, []).

do_read(Port, Output) ->
    receive
        {Port, {data, {eol, Data}}} ->
            io:format("Data: ~p~n",[Data]),
            do_read(Port, [Data|Output]);
        {Port, eof} ->
            io:format("Done reading~n"),
            lists:reverse(Output);
        Any ->
            io:format("No match fifo_client:do_read/1, ~p~n",[Any]),
            do_read(Port, Output)
  end.
