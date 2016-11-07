%% @author billcyz
%% @doc @todo Add description to socket_test.

%% Script name: socket_test.erl
-module(socket_test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).

start(Port) ->
	{ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, 
                                        {active, false}]),
	{ok, LSock}.

%% ====================================================================
%% Internal functions
%% ====================================================================


