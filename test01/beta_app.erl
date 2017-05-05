%% @author billcyz
%% @doc @todo Add description to beta_app.


-module(beta_app).
-behaviour(application).
-export([start/2, stop/1]).


-export([]).

-define(MAX_RESTART, 2).
-define(MAX_SECONDS, 60).

%% Start normal application client
%% start_app_client() ->
%% 	supervisor:start_child(app_client_sup, []).

get_port() ->
	{ok, Port} = application:get_env(beta_app, port),
	Port.

start(normal, _Args) ->
	%%{ok, Port} = application:get_env(beta_app, port),
	{ok, SupPid} = beta_app_sup:start_link(), %% application main supervisor
	true = register(beta_app, self()),
	beta_app_sup:start_child_sup(beta_listener, beta_app_socket_sup, 
								 [get_port()]),
	{ok, SupPid}.

stop(_State) -> ok.

%% init([Port]) ->
%% 	{ok, {one_for_one, ?MAX_RESTART, ?MAX_SECONDS},
%% 	 [{beta_app_socket_sup,
%% 	   {beta_app_socket_sup, start_link, [Port]},
%% 	   infinity, supervisor,
%% 	   [beta_app_socket_sup]}]}.


