%% @author billcyz
%% @doc @todo Add description to knk_udp_listener_sup.


-module(knk_udp_listener_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/1]).

%% ---------------------------------------------------------------------------

%% Start UDP listener supervisor
start_link(KNKPort) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [KNKPort]).

init([KNKPort]) ->
	{ok, {{one_for_one, 2, 10},
		  [{knk_udp_listener,
			{knk_udp_listener, start_link, [KNKPort]},
			permanent, 1000, worker, [knk_udp_listener]}]}}.

%% start_knk_socket() ->
%% 	[start_socket() || _ <- lists:seq(1, 5)].
%% 
%% start_socket() ->
%% 	supervisor:start_child(?MODULE, []).


