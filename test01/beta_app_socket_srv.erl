%% @author billcyz
%% @doc @todo Add description to beta_app_socket_srv.


-module(beta_app_socket_srv).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([start_link/2]).

-record(state, {ip, port, socket, loop}).

start_link(IPAddr, Port) ->
	gen_server:start_link(?MODULE, [IPAddr, Port], []).

init([IPAddr, Port]) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false},
										 {reuseaddr, true}]),
	start_socket(Socket),
	{ok, #state{ip = IPAddr, port = Port, socket = Socket}}.

start_socket(Socket) ->
	case gen_tcp:accept(Socket) of
		{ok, S} ->
			recv_socket(S),
			start_socket(Socket);
		{error, R} -> R
	end.

recv_socket(Socket) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, Data} ->
			%%{SIP, Port, Msg} = process_data(is_binary(Data)),
			case Data of
				<<"Hello">> ->
					gen_server:call(?MODULE, {request, hello});
				<<"Hi">> ->
					gen_server:call(?MODULE, {request, hi})
			end,
			recv_socket(Socket);
		{tcp_closed, S} ->
			io:format("Socket ~w closed [~w]~n", [S, self()]),
			ok;
		{tcp_error, Socket, E} -> E
	end.

%% need to care about node condition
handle_call({request, hello}, _From, S = #state{socket = Socket}) ->
	io:format("Received Hello message...~n"),
	io:format("Prepare to process data from socket ~p....~n", [Socket]),
	{noreply, S};

handle_call({request, hi}, _From, S = #state{socket = Socket}) ->
	io:format("Received Hi message...~n"),
	io:format("Prepare to process data from socket ~p....~n", [Socket]),
	{noreply, S}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Process data to get source ip, source port, and message
%% process_data(Data) ->
%% 	<<SI, SP, Msg>> = Data,
	
	





