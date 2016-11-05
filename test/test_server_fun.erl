%% @author billcyz
%% @doc @todo Add description to test_server_fun.

%% Script name: test_server_fun.erl
-module(test_server_fun).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add/1, double/1]).

add(0) -> 0;
add(X) ->
	X + add(X - 1).

double(X) ->
	X * 2.

%% ====================================================================
%% Internal functions
%% ====================================================================


