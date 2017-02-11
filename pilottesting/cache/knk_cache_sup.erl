%% @author billcyz
%% @doc @todo Add description to knk_cache_sup.


-module(knk_cache_sup).
-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

start() ->
	supervisor:start_child({local, ?SERVER}, ChildSpec)
