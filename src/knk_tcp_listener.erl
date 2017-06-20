%% @author billcyz
%% @doc @todo Add description to knk_tcp_listener.


-module(knk_tcp_listener).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1]).

-record(state, {socket}).

%% ---------------------------------------------------------------------------

%% Start TCP server for client application
start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
	{ok, #state{socket = Socket}, 0}.

handle_info(timeout, #state{socket= Socket} = State) ->
	{ok, ASocket} = gen_tcp:accept(Socket),
	inet:setopts(ASocket, [{active, once}]),
	knk_tcp_listener_sup:start_socket(),
	{noreply, State};
handle_info({tcp, Socket, RawData}, State) ->
	inet:setopts(Socket, [{active, once}]),
	io:format("Received data ~p on socket ~p~n", [RawData, Socket]),
	gen_tcp:send(Socket, io_lib:fwrite("You sent: ~p~n", [RawData])),
	{noreply, State};
handle_info({tcp_closed, Socket}, State) ->
	io:format("~p closed", [Socket]),
	{stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
	io:format("~p error, stopping~n", [Socket]),
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

handle_call(_Request, _From, State) -> {reply, ignored, State}.

handle_cast(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
