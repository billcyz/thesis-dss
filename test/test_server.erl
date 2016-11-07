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

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init(Port) ->
	case is_integer(Port) of
		true ->
			start_server(Port);
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

start_server(Port) ->
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, LSocket} ->
			io:format("the db is listening to ~p~n", [LSocket]),
			server_accept(LSocket);
		{error, Reason} ->
			{stop, Reason}
	end.

server_accept(LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	Pid = spawn(fun() -> loop() end),
	gen_tcp:controlling_process(Socket, Pid).

loop() ->
	receive
		{request, add, X} ->
			gen_server:call(?MODULE, {add, X});
		{request, double, X} ->
			gen_server:call(?MODULE, {double, X})
	end.






