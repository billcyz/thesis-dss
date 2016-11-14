%% @author billcyz
%% @doc @todo Add description to demo_client.

%% Script name: demo_client.erl
-module(demo_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([check_server/0, calculate/1]).

check_server() ->
	%% check server process
	case whereis(demo_server) of
		undefined -> no;
		Name -> {ok, Name}
	end.

calculate(X) ->
	demo_server ! {self(), add, X},
	receive
		{result, Result} ->
			io:format("the result is ~p~n", [Result])
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


