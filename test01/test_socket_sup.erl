%% @author billcyz
%% @doc @todo Add description to test_socket_sup.

%% Test echo server supervisor

-module(test_socket_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/1, start_socket/0]).

-define(MAX_RESTART, 2).
-define(MAX_SECONDS, 60).
-define(SERVER, ?MODULE).
-define(PORT, 10000).
%%-define(IPAddr, {192,168,1,100}).

start_link(Port) ->
	io:format("start_link~n"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false},
										 {reuseaddr, true}]),
	spawn(fun() -> test_socket_srv:start_app_socket() end),
	io:format("init~n"),
	{ok, {{simple_one_for_one, ?MAX_RESTART, ?MAX_SECONDS},
		  [{test_socket_srv,
			{test_socket_srv, start_link, [Port, Socket]},
			temporary, brutal_kill, worker,
			[test_socket_srv]}]}}.

start_socket() ->
	supervisor:start_child(?SERVER, []).




