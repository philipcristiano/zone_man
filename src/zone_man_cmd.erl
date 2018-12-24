-module(zone_man_cmd).
-compile({parse_transform, lager_transform}).

-export([run/2,
         get_vnic/1,
         create_vnic/2,
         list_zones/0,
         parse_machine_zone/1]).

list_zones() ->
    MachineZones = zone_man_cmd:run("/usr/sbin/zoneadm", ["list", "-p", "-c"]),
    Zones = parse_machine_zones(MachineZones),
    Zones.

get_vnic(Name) when is_binary(Name) ->
    get_vnic(binary_to_list(Name));
get_vnic(Name) when is_list(Name) ->
    Result = zone_man_cmd:run(
        "/usr/sbin/dladm", ["show-vnic", "-p", "-o", "link", "dev0"]),
    #{name => Result}.

create_vnic(Link, Name) when is_list(Link) and is_list(Name) ->
    Args = ["create-vnic", "-l", Link, Name],
    Res = zone_man_cmd:run("/usr/sbin/dladm", Args),
    lager:info("Create vnic ~p ~p", [{Args}, {Res}]),
    ok.

parse_machine_zones(MachineZones) ->
    parse_machine_zones(MachineZones, []).

parse_machine_zones([], Zones) ->
    Zones;
parse_machine_zones([H|T], Zones) ->
    Zone = parse_machine_zone(H),
    parse_machine_zones(T, [Zone|Zones]).

parse_machine_zone(MZ) ->
    case re:split(MZ, ":") of
        [ID, Name, Status, Path, UUID, Brand, IPType] ->
            [{id, ID},
             {name, Name},
             {status, Status},
             {path, Path},
             {uuid, UUID},
             {brand, Brand},
             {iptype, IPType}];
        [ID, Name, Status, Path, UUID, Brand, IPType, DebugID] ->
            [{id, ID},
             {name, Name},
             {status, Status},
             {path, Path},
             {uuid, UUID},
             {brand, Brand},
             {iptype, IPType},
             {debugid, DebugID}]
    end.



run(CMD, Options) ->
    lager:info("Run: ~p ~p", [{CMD}, {Options}]),
    Port = open_port({spawn_executable, CMD}, [{line, 4096},
                                               eof,
                                               exit_status,
                                               {args, Options}]),
    do_read(Port, []).

do_read(Port, Output) ->
    receive
        {Port, {data, {eol, Data}}} ->
            do_read(Port, [Data|Output]);
        {Port, eof} ->
            lists:reverse(Output);
        {Port, {exit_status, Status}} ->
            lager:info("Exit status: ~p", [Status]),
            do_read(Port, Output);
        Any ->
            lager:info("No match fifo_client:do_read/1, ~p", [Any]),
            do_read(Port, Output)
  end.
