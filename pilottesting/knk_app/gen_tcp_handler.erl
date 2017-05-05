%% @author billcyz
%% @doc @todo Add description to gen_tcp_handler.


-module(gen_tcp_handler).
-behaviour(gen_server).
-export([start_link/2, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3, init/1]).

-record(state, {supervisor :: pid(),
				handler :: atom(),
				lsocket :: term(),
				socket :: term(),
				state :: term()}).

%% ---------------------------------------------------------------------

%% Start gen_server
start_link(LSocket, HandlerModule) ->
	gen_server:start_link(?MODULE, [self(), LSocket, HandlerModule], []).

init([Supervisor, LSocket, HandlerModule]) ->
	{ok, #state{supervisor = Supervisor, handler = HandlerModule,
				lsocket = LSocket}, 0}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, #state{supervisor = Supervisor, handler = HandlerModule,
							lsocket = LSocket} = State) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			%% Start new child to wait for the next connection.
			supervisor:start_child(Supervisor, []),
			case HandlerModule:handle_accept(Socket) of
				{ok, HandlerState} ->
					{noreply, State#state{socket = Socket,
										  state = HandlerState}};
				{stop, Reason} ->
					{stop, {handl_accept_error, Reason}, State};
				_ ->
					{stop, {handl_accept_error, bad_return}, State}
			end;
		{error, Reason} ->
			{stop, {gen_tcp_accept_error, Reason}, State}
	end;

handle_info({tcp, Socket, Data}, #state{handler = HandlerModule,
										socket = Socket,
										state = HandlerState} = State) ->
	inet:setopts(Socket, [{active, once}]),
	case HandlerModule:handle_tcp(Socket, Data, HandlerState) of
		{ok, NewHandlerState} ->
			{noreply, State#state{state = NewHandlerState}};
		{error, Reason} ->
			{stop, {handle_tcp_error, Reason}, State};
		_ ->
			{stop, {handle_tcp_error, bad_return}, State}
	end;

handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
	{stop, {tcp_error, Reason}, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, #state{handler = HandlerModule, socket = Socket,
						 state = HandlerState}) ->
	if
		Socket /= undefined ->
			gen_tcp:close(Socket);
		true ->
			ok
	end,
	HandlerModule:handle_close(Socket, Reason, HandlerState),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.






