%% @author billcyz
%% @doc @todo Add description to com_db.

%% Script name: com_db.erl

-module(com_db).
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).

start() ->
	application:start(?MODULE).

stop() ->
	application:stop(?MODULE).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start(_Type, _Args) ->
	com_db_sup:start_link().

stop(_State) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


