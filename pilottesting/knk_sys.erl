%% @author billcyz
%% @doc @todo Add description to knk_sys.


-module(knk_sys).


-export([server_register/0]).

%% ------------------------------------------------------------------

%% Read config file knk.conf using shell script

read_config(File) ->
	os:cmd(Command,)
	ok.

%% Server register

server_register() ->
	Node_Name = node(),
	Host_Addr = get_host_addr(),
	{Node_Name, Host_Addr}.

%% ------------------------------------------------------------------

%% Get ip address of server (ipv4)

get_host_addr() ->
	{ok, Addrs} = inet:getifaddrs(),
	hd([
		Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
				size(Addr) == 4, Addr =/= {127,0,0,1}
	   ]).

