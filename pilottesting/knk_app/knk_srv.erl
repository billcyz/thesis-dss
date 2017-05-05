%% @author billcyz
%% @doc @todo Add description to knk_srv.


-module(knk_srv).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([]).

-define(SERVER, ?MODULE).

-record(state, {app,
				status}). %% (running, active, new, down)

%% -----------------------------------------------------------------------------------------

%% Start knk system.
start_link(Socket) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Socket, []).

%% Check knk system current status.
%% Four system status is used: Running, Active, New, and Down.
check_status() ->
	gen_server:call(?SERVER, {sys, check_system_status}).

%% Update knk system status
update_status(Status) ->
	case lists:member(Status, ["Running", "Active", "New", "Down"]) of
		true -> gen_server:call(?SERVER, {sys, update_status, Status});
		false -> {error, wrong_status}
	end.

init(Socket) ->
	<<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
	rand:seed({A, B, C}),
	gen_server:cast(self(), self_init),
	{ok, #state{socket = Socket}}.

handle_call(_, _From, State) ->
	{no_reply, State}.

%% Handle self initialize request. 
handle_cast(self_init, S = #state{socket = Socket}) ->
	{ok, ASocket} = gen_tcp:accept(Socket),
	knk_srv_sup:start_socket(),
	knk_sup:start_child_sup({local, knk_comp_sup}, knk_comp_sup, [ASocket]),
	{ok, S}. %% get app from application environment, and update state

handle_call({register, CompInfo}, S = #state{socket = Socket, app = AppName,
											 status = }) ->
	1;

handle_call({sys, check_system_status}, _From, S = #state{status = Status}) ->
	{reply, {knk_status, [Status]}, S};

handle_call({sys, update_status, Status}, _From, State) ->
	{reply, {status_updated}, State#state{status = Status}}.
	




