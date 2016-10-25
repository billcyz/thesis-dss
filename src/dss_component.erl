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

-module(dss_component).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

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
	ets:new(Tab, Opt).


%% ====================================================================
%% Internal functions
%% ====================================================================



