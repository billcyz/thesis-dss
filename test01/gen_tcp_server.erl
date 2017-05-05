%% @author billcyz
%% @doc @todo Add description to gen_tcp_server.


-module(gen_tcp_server).

-export([start_link/2, start_link/3, stop/1]).

%% ------------------------------------------------------------------

%% gen_tcp_server callback definition
-callback handle_accept(Socket :: term()) ->
	{ok, State :: term() | {stop, Reason :: term()}}.

-callback handle_tcp(Socket :: term(), Data :: binary(), State :: term()) ->
	{ok, NewState :: term() | {stop, Reason :: term()}}.

-callback handle_close(Socket :: term(),
						Reason :: normal | {tcp_error, term()} |
									{handle_accept_error, term()} |
									{handle_tcp_error, term()},
						State :: term()) -> ok.

%% Start gen_tcp_server with default options
%% start_link(HandlerModule, Port)
start_link(HandlerModule, Port) ->
	start_link(HandlerModule, Port, []).

%% Start gen_tcp_server with custom options
%% start_link(HandlerModule, Port, Opts)
start_link(HandlerModule, Port, Opts) ->
	{ok, SupPid} = gen_tcp_server_sup:start_link(HandlerModule, 
												 Port, Opts),
	N = case lists:keyfind(pool, 1, Opts) of
			false -> 1;
			{pool, PoolSize} ->
				PoolSize
		end,
	[{ok, _} = supervisor:start_child(SupPid, []) || _ <- lists:seq(1, N)],
	{ok, SupPid}.

stop(Pid) ->
	exit(Pid, normal).


