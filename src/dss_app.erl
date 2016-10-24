%% @author billcyz
%% @doc @todo Add description to dss_app.


-module(dss_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

start(_StartType, _StartArgs) ->
	case application:get_env(dss, Par) of
		{ok, ???} -> 1;
		_ -> ok
	end,
	
	%% start supervisor
	case dss_sup:start_link() of
	end.


stop() ->
	io:format("Stopped application dss.\n"),
	ok.



%% ====================================================================
%% Internal functions
%% ====================================================================


