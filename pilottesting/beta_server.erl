%% @author billcyz
%% @doc @todo Add description to beta_server.


-module(beta_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

start_link() ->
	gen_server:start_link({local, beta_server}, beta_server, [], []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================


%% ====================================================================
%% Internal functions
%% ====================================================================


