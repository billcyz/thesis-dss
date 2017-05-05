%% @author billcyz
%% @doc @todo Add description to test_socket_client.

%% Test for socket connection (echo server)

-module(test_socket_client).

-export([send_msg/3]).

%% Establish connection with port number and destination
%% ip address
-spec establish_conn(inet:socket_address(),
					 inet:port_number()) -> 'ok'.
establish_conn(HostAddr, Port) ->
	gen_tcp:connect(HostAddr, Port, [binary, 
									 {active, false},
									 {send_timeout, 5000}]).

%% Send message
send_msg(HostAddr, Port, Data) ->
	{ok, Socket} = establish_conn(HostAddr, Port),
	gen_tcp:send(Socket, Data),
	ok.

