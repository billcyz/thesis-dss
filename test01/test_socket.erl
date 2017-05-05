%% @author billcyz
%% @doc @todo Add description to test_socket.


-module(test_socket).


-export([start/1]).


start(Port) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false},
										 {reuseaddr, true}]),
	{ok, Socket}.
	%%start_socket(Socket).

start_socket(Socket) ->
	case gen_tcp:accept(Socket) of
		{ok, S} ->
			io:format("Accepting socket ~p~n", [S]);
		{error, R} -> R
	end.