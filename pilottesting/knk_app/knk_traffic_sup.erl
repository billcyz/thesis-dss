%% @author billcyz
%% @doc @todo Add description to knk_traffic_sup.


-module(knk_traffic_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/3]).

-include("knk_socket.hrl").

-define(SERVER, ?MODULE).


%% -------------------------------------------------------------------------

%% Start the handler supervisor
start_link(HandlerModule, Fun, Args) ->
	supervisor:start_link(?MODULE, [normal, HandlerModule, Fun, Args]).

%% Start user defined udp port for receiving broadcast message
start_bport(HandlerModule, Port, UserOpts) ->
	supervisor:start_link(?MODULE, [port, HandlerModule, Port, ?BROADCAST_PORT, UserOpts]).

init([normal, HandlerModule, Fun, Args]) ->
	{ok, {{simple_one_for_one, 0, 1},
		  [{knk_traffic_handler,
			{knk_traffic_handler, start_link, [HandlerModule, Fun,
											   Args]},
			temporary, infinity, worker},
		   [knk_traffic_handler]]}}.

init([port, HandlerModule, Port, ?BROADCAST_PORT, UserOpts]) ->
	Opts = SERVER_OPT ++ UserOpts,
	{ok, Socket} = gen_udp:open(?BROADCAST_PORT, remove_opts(Opts)), %% start broadcst port
	{}
