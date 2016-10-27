%% @author billcyz
%% @doc @todo Add description to com_db_sup.

%% Script name: com_db_sup.erl

-module(com_db_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-define(CHILD(I, Type, Timeout, Args), {I, {I, start_link, Args}, permanent, Timeout, Type, [I]}).
-define(CHILD(I, Type, Timeout), ?CHILD(I, Type, Timeout, [])).
-define(CHILD(I, Type), ?CHILD(I, Type, 5000)).

start() ->
	spawn(fun() ->
				  supervisor:start_link({local, ?MODULE}, ?MODULE, [])
		  end).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
	Children = lists:flatten(
				 [?CHILD(com_db_srv, worker)]),
	
	{ok, {{one_for_one, 10, 10}, Children}}.


%% ====================================================================
%% Internal functions
%% ====================================================================


