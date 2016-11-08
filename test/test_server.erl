%% @author billcyz
%% @doc @todo Add description to test_server.

%% Script name: test_server.erl
-module(test_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).

-define(TCP_OPTIONS, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]).

-record(server_state, {
					   port,
					   loop,
					   ip = any,
					   lsocket = null,
					   conn = 0,
					   maxconn}).

start(Port) ->
	State = #server_state{port=Port},
	gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init(State = #server_state{port=Port}) ->
	case is_integer(Port) of
		true ->
			start_server(State);
		false ->
			{error, port_not_right}
	end.

handle_call({add, X}, _From, State) ->
	Reply = case test_server_fun:add(X) of
				{ok, Result} ->
					{result, Result};
				{error, Reason} ->
					Reason
			end,
	{reply, Reply, State};

handle_call({double, X}, _From, State) ->
	Reply = case test_server_fun:double(X) of
				{ok, Result} ->
					{result, Result};
				{error, Reason} ->
					Reason
			end,
	{reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

start_server(State = #server_state{port=Port}) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, LSocket} ->
			io:format("the db is listening to ~p~n", [LSocket]),
			server_accept(State = #server_state{lsocket=LSocket});
		{error, Reason} ->
			{stop, {create_listen_socket, Reason}}
	end.

server_accept(State = #server_state{lsocket=LSocket, loop=Loop, conn=Conn, maxconn=Max}) ->
	proc_lib:spawn(test_server, accept_loop, [self(), LSocket, Loop, Conn, Max]),  
    State.

accept_loop(Server, LSocket, {M, F}, Conn, Max) ->
	{ok, Sock} = gen_tcp:accept(LSocket),
	if
		Conn + 1 > Max ->
			io:format("reach the max connection~n"),
			gen_tcp:close(Sock);
		true ->
			gen_server:cast(Server, {accept_new, self()}),
			M:F(Sock)
	end.

	receive
		{request, add, X} ->
			gen_server:call(?MODULE, {add, X});
		{request, double, X} ->
			gen_server:call(?MODULE, {double, X})
	end.






