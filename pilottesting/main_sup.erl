%% @author billcyz
%% @doc @todo Add description to main_sup.


-module(main_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
	supervisor:start_link(main_sup, []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
	SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
	ChildSpecs = [#{id => beta_sup,
					start => {beta_sup, start_link, []},
					restart => permanent,
					shutdown => 100000,
					type => supervisor,
					modules => [beta_sup]}],
	{ok, {SupFlags, ChildSpecs}}.


%% ====================================================================
%% Internal functions
%% ====================================================================


