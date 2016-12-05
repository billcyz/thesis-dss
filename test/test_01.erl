%% @author billcyz
%% @doc @todo Add description to test_01.


-module(test_01).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new_cache_tab/3, new_cache_table/2]).

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

new_cache_table(Tab, Options) ->
	case ets:info(Tab) of
		[_] ->
			{table_exists};
		undefined ->
			Tid = ets:new(Tab, Options),
			{ok, Tid}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


