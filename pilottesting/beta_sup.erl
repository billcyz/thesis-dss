%% @author billcyz
%% @doc @todo Add description to beta_sup.


-module(beta_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
	supervisor:start_link(beta_sup, []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
	SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
	ChildSpecs = [#{id => beta_main,
					start => {beta_main, start, []},
					restart => permanent,
					shutdown => 100000,
					type => worker,
					modules => [beta_main]}],
	{ok, {SupFlags, ChildSpecs}}.


%% ====================================================================
%% Internal functions
%% ====================================================================


