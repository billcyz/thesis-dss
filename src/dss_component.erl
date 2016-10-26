%% @author billcyz
%% @doc @todo Add description to dss_component.

%% Script name: dss_component.erl
%% This script detects components and create ets tables to store
%% component information. The table will be first examined, if no
%% records detected, one process will be spawned to check component
%% existence, otherwise no actinos be taken.
%% 
%% The component hash function or value should be included for future
%% use.
%%
%% Component database

-module(dss_component).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, check_table_exists/1, create_table/2]).

start(Node, Table) ->
	process_flag(trap_exit, true),
	register(dss_com, spawn(?MODULE, init, [Node, Table])),
	ok.

check_table_exists(Table) ->
	case ets:info(Table) of
		undefined ->
			%% Table does not exists
			{table_not_exists};
		_True ->
			%% Table exists
			{table_exists}
	end.

create_table(Tab, Opt) ->
	ets:new(Tab, Opt),
	case ?MODULE:check_table_exists(Tab) of
		{table_not_exists} ->
			not_ok;
		{table_exists} ->
			ok
	end.

%% Create component table for the system
init_table(Tab) ->
	?MODULE:create_table(Tab, [named_table, protected]),
	ets:insert(Tab, {com_01, 0}),
	ets:insert(Tab, {com_02, 0}),
	ets:insert(Tab, {com_03, 0}).

%% Distribute components


%% ====================================================================
%% Internal functions
%% ====================================================================

update_table(Table) ->
	1.

