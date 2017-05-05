%% @author billcyz
%% @doc @todo Add description to knk_comp_sup.


-module(knk_comp_sup).
-behaviour(supervisor).
-export([init/1]).


-export([]).

start_link() ->
	supervisor:start_link({local, knk_comp_sup}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 60, 6000}, 
		  [{knk_comp_srv,
			{knk_comp_srv, start_link, []},
			transient, 1000, worker, [knk_comp_srv]}]}}.


