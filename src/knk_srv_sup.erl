%% @author billcyz
%% @doc @todo Add description to knk_srv_sup.

%% KNK server supervisor

-module(knk_srv_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/2]).

%% --------------------------------------------------------------------------

start_link(CompNode, AppName) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [CompNode, AppName], []).

init([CompNode, AppName]) ->
	{ok, {{one_for_all, 5, 60},
		  [{knk_srv, {knk_srv, start_link, [CompNode, AppName]},
			permanent, 5000, worker, [knk_srv]}]}}.
