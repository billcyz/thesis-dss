%% @author billcyz
%% @doc @todo Add description to com_db_sup.

%% Script name: com_db_sup.erl

-module(com_db_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init(FileName) ->
	1.

%% ====================================================================
%% Internal functions
%% ====================================================================


