%% @author billcyz
%% @doc @todo Add description to test_socket_srv.

%% Test echo server

-module(test_socket_srv).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, start_app_socket/0]).

-record(state, {socket}).

start_link(Port, Socket) ->
	gen_server:start_link(?MODULE, [Port, Socket], []).

start_app_socket() ->
	[test_socket_sup:start_socket() || _ <- lists:seq(1, 5)].

init([Port, Socket]) ->
	io:format("Starting socket ~p on port ~p~n", [Socket, Port]),
	{ok, #state{socket = Socket}, 0}.

%% need to care about node condition
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, #state{socket= Socket} = State) ->
	{ok, ASocket} = gen_tcp:accept(Socket),
	inet:setopts(ASocket, [{active, once}]),
	io:format("~p accepted~n", [ASocket]),
	test_socket_sup:start_socket(),
	{noreply, State};

handle_info({tcp, Socket, RawData}, State) ->
	inet:setopts(Socket, [{active, once}]),
	io:format("Received data ~p on socket ~p~n", [RawData, Socket]),
	{noreply, State};

handle_info({tcp_closed, Socket}, State) ->
	io:format("~p closed", [Socket]),
	{stop, normal, State};

handle_info({tcp_error, Socket}, State) ->
	io:format("~p error, stopping~n", [Socket]),
	{stop, normal, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



