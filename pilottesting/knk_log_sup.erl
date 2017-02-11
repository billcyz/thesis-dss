%% @author billcyz
%% @doc @todo Add description to knk_log_sup.

%% Supervisor of knk logging.

-module(knk_log_sup).
-behaviour(supervisor).
-export([init/1]).


-export([]).

init([]) ->
	{ok, {{one_for_all, 0, 1}, []}}.

