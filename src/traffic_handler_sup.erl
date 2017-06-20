%% @author billcyz
%% @doc @todo Add description to traffic_handler_sup.


-module(traffic_handler_sup).
-behaviour(supervisor).
-export([init/1]).

-export([]).

-define(SERVER, ?MODULE).

%% ---------------------------------------------------------------------

%% Start traffic handler supervisor
start_link(NodeName, AppName, Port, BAddr) ->
	supervisor:start_link(?MODULE, [NodeName, AppName, Port, BAddr]).

init([NodeName, AppName, Port, BAddr]) ->
	
