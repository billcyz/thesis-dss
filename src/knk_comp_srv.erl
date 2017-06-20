%% @author billcyz
%% @doc @todo Add description to knk_comp_srv.

%% Should work in the hidden node.

-module(knk_comp_srv).
-behaviour(gen_server).

-export([init/1, handle_call/3, 
		 handle_cast/2, handle_info/2, 
		 terminate/2, code_change/3]).


-export([get_cookie/3]).

-define(SERVER, ?MODULE).

%% -record(state, {app,
%% 				socket,
%% 				next}).

-record(state, {tab_init_status}).

%% ------------------------------------------------------------------------------

%% Start component server
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ok -> {info, local_storage_initialized};
init([]) ->
	process_flag(trap_exit, true),
	knk_log:write_log(
	  normal, lists:flatten(io_lib:format("Starting component server...~n", 
														  []))),
	ok = knk_comp:ets_init_table(),
	{ok, #state{tab_init_status = initialized}}.

%% Stop component server
%% stop() ->
%% 	gen_server:cast(?SERVER, stop).

%% Register component [AppName, CompName, NodeName, FunList]
register_component([AppName, CompName, NodeName, [FunList]]) ->
	gen_server:call(knk_comp_srv, {register, [AppName, CompName,
											  NodeName, [FunList]]}, 5000).

%% Component communication
call_comp_func(AppName, CompName, FunName) ->
	gen_server:call(comp_srv, {call_comp_func, {AppName, CompName,
												FunName}}, 5000).

distribute_comp(KNode) ->
	gen_server:call(comp_srv, {distribute_comp, KNode}, infinity).


%% Distribute components into servers.
%% distribute_component(NodeName) ->
%% 	gen_server:call(comp_srv, {distribute_component, NodeName}, 5000).

%% Decides which component should be distributed into servers.
vote_comp_candidates() ->
	1.

%% Test component connection
test_conn(NodeName, CompName) ->
	gen_server:call(comp_srv, {test_node_conn, NodeName, CompName},
					 5000).

%% Create test node


%% -------------------------------------------------------------------------------
%% Handle call

handle_call({test_node_conn, NodeName, CompName}, _From, _State) ->
	{ok, Cookie} = knk_comp:get_cookie(NodeName, CompName),
	case knk_comp:test_comp_conn(NodeName, Cookie) of
		ok -> {info, conn_successful};
		{error, E} -> E
	end;

handle_call({distribute_comp, NodeName}, _From, _State) ->
	[] = knk_comp:select_comp(),
	{ok, CompName} = vote_comp_candidates(),
	1;

handle_call({register, [AppName, CompName, NodeName, FunList]}, 
			From, #state{app=AppName}) ->
	case knk_comp:ets_check() of
		{error, tab_not_exist} ->
			knk_comp_sup ! restart,
			io:format("Restarting component server...~n");
		_ ->
			case knk_comp:register_comp([AppName, 
										 CompName, NodeName, FunList]) of
				ok -> From ! Msg %% send back success message to node and update component count
			end
	end.

handle_cast({talk, Msg}, #state={app=AppName, socket=Socket}) ->
	[{}] = Msg,
	1.

handle_call({call_comp_func, {AppName, CompName, FunName}}, From, _State) ->
	1.



%% -------------------------------------------------------------------------------

%% Synchronize component storage information
comp_sync() ->
	[_] = 

handle_cast(stop, _State) ->
	{stop, normal, _State}.

terminate(normal, _State) ->
	ok.