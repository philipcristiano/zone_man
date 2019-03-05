-module(zone_man_cmd).
-compile({parse_transform, lager_transform}).

-export([run/2,
         get_vnic/1,
         create_vnic/2,
         get_zone_cfg/1,
         configure_zone/3,
         list_zones/0,
         parse_machine_zone/1]).

list_zones() ->
    {0, MachineZones} = zone_man_cmd:run(
        "/usr/sbin/zoneadm", ["list", "-p", "-c"]),
    Zones = parse_machine_zones(MachineZones),
    Zones.

get_vnic(Name) when is_binary(Name) ->
    get_vnic(binary_to_list(Name));
get_vnic(Name) when is_list(Name) ->
    Returned = zone_man_cmd:run(
        "/usr/sbin/dladm", ["show-vnic", "-p", "-o", "link", "dev0"]),

    MissingStr = "dladm: invalid vnic name '" ++ Name ++ "': object not found",
    lager:debug("MissingStr ~p", [MissingStr]),
    Return = case Returned of
        {0, Result} -> #{name => Result};
        {1, MissingStr} -> undefined
    end,
    Return.

create_vnic(Link, Name) when is_list(Link) and is_list(Name) ->
    Args = ["create-vnic", "-l", Link, Name],
    {0, _Result} = zone_man_cmd:run("/usr/sbin/dladm", Args),
    ok.

get_zone_cfg(Name) when is_binary(Name) ->
    get_zone_cfg(binary_to_list(Name));
get_zone_cfg(Name) when is_list(Name) ->
    Name,

    Returned = zone_man_cmd:run("/usr/sbin/zonecfg", ["-z", Name, "info"]),
    MissingStr = Name ++ ": No such zone configured",
    lager:info("zonecfg returned: ~p", [Returned]),

    Return = case Returned of
        {1, MissingStr} -> undefined;
        _ -> ok
    end,

    Return.

configure_zone(Name, ZonePath, []) ->
    Args0 = ["create;", " zonepath=", ZonePath, ";"],
    Args1 = Args0 ++ [" commit"],
    ArgString = lists:flatten(Args1),

    Returned = zone_man_cmd:run("/usr/sbin/zonecfg", ["-z", Name, ArgString]),
    Return = case Returned of
        _ -> ok
    end,
    Return.

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
    do_read(Port, [], undefined).

do_read(Port, Output, ExitCode) ->
    receive
        {Port, {data, {eol, Data}}} ->
            do_read(Port, [Data|Output], ExitCode);
        {Port, eof} ->
            {ExitCode, lists:reverse(Output)};
        {Port, {exit_status, Status}} ->
            lager:info("Exit status: ~p", [Status]),
            do_read(Port, Output, Status);
        Any ->
            lager:info("No match fifo_client:do_read/1, ~p", [Any]),
            do_read(Port, Output, ExitCode)
  end.
