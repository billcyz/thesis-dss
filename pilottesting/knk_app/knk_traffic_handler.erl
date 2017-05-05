%% @author billcyz
%% @doc @todo Add description to knk_traffic_handler.


-module(knk_traffic_handler).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).

-record(state, {supervisor :: pid(),
				handler :: atom(),
				state :: term(),
				self :: node()}).

%% ---------------------------------------------------------------------------------------

%% Start traffic handler server
start_link(HandlerModule, Fun, Args) ->
	gen_server:start_link(?MODULE, [self(), HandlerModule, Fun, Args], []).

init([SupPid, HandlerModule, Fun, Args]) ->
	{ok, #state{supervisor = SupPid, handler = HandlerModule,
				state = {[HandlerModule, Fun, Args], node()}}}.

handle_call(Request, _From, #state{supervisor = Supervisor, 
									handler = HandlerModule, state = State,
									self = Self}) ->
	case HandlerModule:handle_traffic
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{reply, ok, State}.