%% @author billcyz
%% @doc @todo Add description to gen_tcp_server_sup.

%% Similar to https://github.com/rpt/gen_tcp_server

-module(gen_tcp_server_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/3]).

-include("gen_tcp_server.hrl").

-define(SERVER, ?MODULE).

%% Start the handler supervisor
start_link(HandlerModule, Port, UserOpts) ->
	supervisor:start_link(?MODULE, [HandlerModule, Port, UserOpts]).

init([HandlerModule, Port, UserOpts]) ->
	%% Open listening socket
	Opts = ?GEN_TCP_SERVER_OPT ++ UserOpts,
	{ok, LSocket} = gen_tcp:listen(Port, remove_opts(Opts)),
	{ok, {{simple_one_for_one, 0, 1},
		  [{gen_tcp_handler,
			{gen_tcp_handler, start_link, [LSocket,
										   HandlerModule]},
			temporary, infinity, worker,
			[gen_tcp_handler]}]}}.

%% Remove custom options
remove_opts(Opts) ->
	remove_opts(Opts, Opts).

remove_opts([], Opts) ->
	Opts;
remove_opts([{pool, _} | Rest], _Opts) ->
	remove_opts(Rest, Rest);
remove_opts([_ | Rest], Opts) ->
	remove_opts(Rest, Opts).
