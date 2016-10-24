%% @author billcyz
%% @doc @todo Add description to dss_gen_server.

%% I'm a Chinese, please forgive my poor English

%% This programe will start the server, and two hidden nodes.
%% one hidden node is for exception logging, another hidden node
%% set up the database for components. The database is used for
storing component distribution information on its local server. 

-module(dss_gen_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% start dss server
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% start hiden node_01 (distribution)
start_hid01_node() ->
	case net_kernel:start([hid_01, shortnames]) of
		{ok, Pid} -> Pid;
		{error, Reason} -> Reason
	end.

%% start hidden node_02 (exception)
start_hid02_node() ->
	case net_kernel:start([hid_02, shortnames]) of
		{ok, Pid} -> Pid;
		{error, Reason} -> Reason
	end.

%%init()


%% ====================================================================
%% Behavioural functions
%% ====================================================================


%% ====================================================================
%% Internal functions
%% ====================================================================


