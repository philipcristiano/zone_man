-module(zone_man_cmd).

-export([run/2]).



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
