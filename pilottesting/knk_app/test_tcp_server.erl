%% @author billcyz
%% @doc @todo Add description to test_tcp_server.


-module(test_tcp_server).
-behaviour(gen_tcp_server).
-export([handle_tcp/3, handle_close/3, handle_accept/1]).

-export([start/1, send_msg/3]).

%% ---------------------------------------------------------------------

-record(state, {}).

start(Port) ->
	gen_tcp_server:start_link(?MODULE, Port).

%% stop() ->
%% 	gen_tcp_server:stop(self()).

send_msg(IPAddr, Port, Data) ->
	{ok, Socket} = gen_tcp:connect(IPAddr, Port, 
								   [binary, {active, false}]),
	%%Data = io_lib:format("Message from ~p : ~p", [IPAddr, Port]),
	gen_tcp:send(Socket, Data).

handle_tcp(Socket, Data, State) ->
	%%<<SIP:4/binary, PN:16/integer, Rest/binary>> = Data,
	NewData = <<"asdasdasd", Data/binary>>,
	gen_tcp:send(Socket, NewData),
	{ok, State}.

handle_close(_Socket, _Reason, _State) ->
	ok.

handle_accept(_Socket) -> 
	{ok, #state{}}.
