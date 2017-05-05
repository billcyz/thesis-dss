%% @author billcyz
%% @doc @todo Add description to ets_sup.


-module(ets_sup).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/1]).

-export([]).

start_link(App) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [App]).

init([App]) ->
	{ok, {{one_for_one, 1, 5}, 
		  [{ets_server,
			{ets_server, start, [App]},
			transient, 10, worker, [ets_server]}]}}.




