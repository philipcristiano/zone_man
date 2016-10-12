-module(zone_man_cmd).

-export([run/2,
         run_interactive/2,
         close_port/1,
         do_read_start_interactive/2,
         send_to_port/2,
         list_zones/0,
         parse_machine_zone/1]).

list_zones() ->
    MachineZones = zone_man_cmd:run("/usr/sbin/zoneadm", ["list", "-p", "-c"]),
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
    Port = start_port(CMD, Options),
    do_read(Port, []).

do_read(Port, Output) ->
    receive
        {Port, {data, {eol, Data}}} ->
            do_read(Port, [Data|Output]);
        {Port, eof} ->
            lists:reverse(Output);
        Any ->
            io:format("No match fifo_client:do_read/1, ~p~n",[Any]),
            do_read(Port, Output)
  end.

run_interactive(CMD, Options) ->
   spawn(?MODULE, do_read_start_interactive, [CMD, Options]).

%% Pid = zone_man_cmd:run_interactive("./echo.py", []).
%% zone_man_cmd:send_to_port(Pid, "Hi").

do_read_start_interactive(CMD, Options) ->
    io:format("starting~n"),
    Port = start_port(CMD, Options),
    do_read_interactive(Port, []).

send_to_port(Pid, Command) ->
   Pid ! {send, Command}.

close_port(Pid) ->
   Pid ! {close_port}.

do_read_interactive(Port, Output) ->
    io:format("read interactive~n"),
    receive
        {Port, {data, {eol, Data}}} ->
            io:format("Found data ~p~n", [Data]),
            do_read_interactive(Port, [Data|Output]);
        {Port, eof} ->
            io:format("Eof ~n"),
            lists:reverse(Output);
        {send, Command} ->
            io:format("Received command ~p~n", [Command]),
            Port ! {self(), {command, Command}},
            do_read_interactive(Port, Output);
        {close_port} ->
            Port ! {self(), close};
        Any ->
            io:format("No match fifo_client:do_read/1, ~p~n",[Any]),
            do_read_interactive(Port, Output)
  end.

start_port(CMD, Options) ->
    open_port({spawn_executable, CMD}, [{line, 4096},
                                        {args, Options}]).

