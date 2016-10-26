%% @author billcyz
%% @doc @todo Add description to node_finder.


%% Script name: node_finder.erl
%% Application to find available nodes by using UDP

-module(node_finder).
-behaviour(application).
-export([start/2, stop/1, start/0, stop/0]).
-export([discover/0]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

discover() ->
	node_finder_server:discover().

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start() ->
	crypto:start(),
	application:start(nodefinder).

start(_Type, _Agrs) ->
	{ok, Addr} = application:get_env(nodefinder, addr),
	{ok, Port} = application:get_env(nodefinder, port),
	{ok, Ttl} = application:get_env(nodefinder, multicast_ttl),
	node_finder_sup:start_link(Addr, Port, Ttl).

stop() ->
	application:stop(nodefinder).

stop(_State) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


