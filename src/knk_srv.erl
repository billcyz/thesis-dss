%% @author billcyz
%% @doc @todo Add description to knk_srv.


-module(knk_srv).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2,
		 check_knk_status/0,
		 update_sys_status/1]).

-define(SERVER, ?MODULE).

-record(state, {app,
				comp_storage, %% component storage node name
				sys_status}). %% (running, active, new)

%% -----------------------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------------------
%% Start knk system with application name
start_link(CompNode, AppName) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [AppName, CompNode], []).

%% Check all status of KNK server
check_knk_status() ->
	gen_server:call(?SERVER, check_knk_status).

%% Update knk system status
update_sys_status(NewStatus) ->
	case lists:member(NewStatus, [running, active, new, down]) of
		true -> gen_server:call(?SERVER, {update_sys_status, NewStatus});
		false -> {error, wrong_status}
	end.

%% Register component
%% Receive component information and store information in local storage
register_comp(CompNode, CompInfo) ->
	gen_server:call(?SERVER, {register_comp, CompNode, CompInfo}).

%% ------------------------------------------------------------------------------------------

init([AppName, CompNode]) ->
	{ok, #state{app = AppName, comp_storage = CompNode, sys_status = new}}.

handle_call(check_knk_status, _From, #state{app = App, comp_storage = CompNode,
											sys_status = SysStatus} = State) ->
	{reply, {node(), App, CompNode, SysStatus}, State};
handle_call({update_sys_status, NewStatus}, _From, State) ->
	{reply, ok, State#state{sys_status = NewStatus}};
handle_call({register_comp, CompNode, CompInfo}, _From, 
			#state{app = App, comp_storage = StorageNode} = State) ->
	[AppName, _] = CompInfo,
	if
		AppName =:= App ->
			Result = traffic_handler:message_map(
					   {sys, [register_comp, [StorageNode, 
											  CompNode, CompInfo]]}),
			Result = gen_server:call({knk_etssrv, StorageNode}, 
									 {register, CompNode, CompInfo}),
			{reply, Result, State};
		true -> {reply, wrong_app, State}
	end;
			
handle_call(_, _From, State) ->
	{no_reply, State}.

handle_info(_Info, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Ext) -> {ok, State}.




