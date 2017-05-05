%% @author billcyz
%% @doc @todo Add description to test_socket_app.


-module(test_socket_app).
-behaviour(application).
-export([start/2, stop/1]).

-export([]).

start(normal, _Args) ->
	{ok, Port} = application:get_env(test_socket, port),
	test_socket_sup:start_link(Port).

stop(_State) -> ok.