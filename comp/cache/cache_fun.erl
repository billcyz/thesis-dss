%% @author billcyz
%% @doc @todo Add description to cache_fun.

%% Script name: cache_fun.erl
-module(cache_fun).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% Create ets table
new_cache_tab(Tab, Type, Acc) ->
	%% Tab = Table name
	%% Type = Table type
	%% Acc = Access type
	case ets:info(Tab) of
		[_] ->
			{table_exists};
		undefined ->
			Tid = ets:new(Tab, [Type, Acc]),
			{ok, Tid}
	end.

new_cache_tab(Tab, Type, Acc, named_table) ->
	case ets:info(Tab) of
		[_] ->
			{table_exists};
		undefined ->
			TabName = ets:new(Tab, [Type, Acc, named_table]),
			{ok, TabName}
	end.

%% Rename table
rename_tab(Tab, Name) ->
	ets:rename(Tab, Name).

%% Get all values in table


%% ====================================================================
%% Internal functions
%% ====================================================================


