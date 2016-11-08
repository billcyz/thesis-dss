%% @author billcyz
%% @doc @todo Add description to test_server_client.

%% Script name: test_server_client.erl
-module(test_server_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_client/1, send_msg/2]).

start_client(Port) ->
	{ok, Socket} = gen_tcp:connect("localhost", Port, [{active, false},
													   {packet,raw}]),
	{ok, Socket}.

send_msg(Port, Message) ->
	case ?MODULE:start_client(Port, Message) of
		{ok, Socket} ->
			gen_tcp:send(Socket, Message),
			A = gen_tcp:recv(Socket,0),
    		gen_tcp:close(Socket),
    		A;
		{error, Reason} ->
			{Reason}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


