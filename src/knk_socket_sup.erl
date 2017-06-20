%% @author billcyz
%% @doc @todo Add description to knk_socket_sup.


-module(knk_socket_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/2]).

-define(CHILD(I, Timeout, Type), 
		{I, {I, start_link, []}, permanent, Timeout, Type, [I]}).
-define(CHILD(I, Args, Timeout, Type), 
		{I, {I, start_link, Args}, permanent, Timeout, Type, [I]}).

%% ------------------------------------------------------------------

%% Start socket supervisor
start_link(AppPort, KNKPort) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [AppPort, KNKPort]).

init([AppPort, KNKPort]) ->
	Child1 = ?CHILD(knk_tcp_listener_sup, [AppPort], infinity, supervisor),
	Child2 = ?CHILD(knk_udp_listener_sup, [KNKPort], infinity, supervisor),
	{ok, {{one_for_one, 5, 10}, [Child1, Child2]}}.

