%% @author billcyz
%% @doc @todo Add description to demo_server.

%% Script name: demo_server.erl
-module(demo_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0, init/0, add/1]).

start() ->
	Pid = spawn(?MODULE, init, []),
	register(demo_server, Pid),
	{demo_server, Pid}.

stop() ->
	demo_server ! stop,
	ok.

init() ->
	receive
		stop -> true;
		{From, add, X} ->
			Result = ?MODULE:add(X),
			From ! {result, Result},
			init()
	end.	

%% ====================================================================
%% Internal functions
%% ====================================================================

add(X) ->
	X + 3.
