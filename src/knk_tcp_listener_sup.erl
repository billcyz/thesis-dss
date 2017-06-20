%% @author billcyz
%% @doc @todo Add description to knk_tcp_listener_sup.


-module(knk_tcp_listener_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/1, start_socket/0]).

%% ---------------------------------------------------------------------

% Start TCP listener supervisor
start_link(AppPort) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [AppPort]).

init([AppPort]) ->
	{ok, Socket} = gen_tcp:listen(AppPort, [binary, 
											{active, false}]),
	spawn_link(fun() -> start_app_socket() end),
	{ok, {{simple_one_for_one, 2, 10},
		  [{knk_tcp_listener,
			{knk_tcp_listener, start_link, [Socket]},
			temporary, 1000, worker, [knk_tcp_listener]}]}}.

start_app_socket() ->
	[start_socket() || _ <- lists:seq(1, 5)].

start_socket() ->
	supervisor:start_child(?MODULE, []).
