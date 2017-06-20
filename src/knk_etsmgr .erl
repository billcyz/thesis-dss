%% @author billcyz
%% @doc @todo Add description to 'knk_etsmgr '.

%% KNK ets management server. It creates ets table for the
%% KNK system. The management server also grants table permission
%% to general ets server which are accessed by the KNK system node.

-module('knk_etsmgr ').
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2]).

-define(SERVER, ?MODULE).
-define(KNKETS, [app, comp, comp_atts, comp_dist,
				 tarhash, selection_pool]).

-record(state, {knk,
				app}).

%% ------------------------------------------------------------------------

%% Start ets management server
start_link(KNKName, AppName) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [KNKName, AppName], []).

%% Create ets table according to table name
init_tab(TabList) ->
	SRV = whereis(knk_etssrv),
	link(SRV),
	knk_comp:create_tab(TabList),
	knk_select_monitor:start_monitor().

init([KNKName, AppName]) ->
	process_flag(trap_exit, true),
	init_tab(?KNKETS),
	{ok, #state{knk = KNKName, app = AppName}}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'ETS-TRANSFER', TableId, _Pid, Data}, State) ->
	SRV = wait_for_srv(),
	link(SRV),
	ets:give_away(TableId, SRV, Data),
	{noreply, State}.

wait_for_srv() ->
	case whereis(knk_etssrv) of
		undefined ->
			timer:sleep(1),
			wait_for_srv();
		Pid -> Pid
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

