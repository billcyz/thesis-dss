%% @author billcyz
%% @doc @todo Add description to knk_udp_listener.


-module(knk_udp_listener).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1, send_msg/3]).

-record(state, {socket, port}).

%% ----------------------------------------------------------------------------------------

start_link(KNKPort) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [KNKPort], []).

%% Send message
send_msg(DstIP, Port, Data) ->
	gen_server:call(?MODULE, {send_msg, DstIP, Port, Data}).

init([KNKPort]) ->
	{ok, Socket} = gen_udp:open(KNKPort, [binary,
										  {active, true},
										  {reuseaddr, true},
										  {broadcast, true}]),
	{ok, #state{socket = Socket, port = KNKPort}}.

handle_info({udp, _Socket, _Addr, _Port, Data}, #state{socket = Socket} = State) ->
	io:format("Server received data ~p from Socket ~p~n", [Data, Socket]),
	{noreply, State}.

handle_call({send_msg, DstIP, Port, Data}, _From, #state{socket = Socket} = State) ->
	gen_udp:send(Socket, DstIP, Port, Data),
	{noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Old, State, _Extra) -> {ok, State}.

