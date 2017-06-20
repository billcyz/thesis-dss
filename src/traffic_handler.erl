%% @author billcyz
%% @doc @todo Add description to tarffic_handler.

%% Traffic handler is the server which receives message from sockets
%% and nodes. It parses each message and choose which operations should
%% be taken according to pre-desinged message pattern.

-module(traffic_handler).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([message_map/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% -----------------------------------------------------------------------------------

%% Start traffic handler server
-spec start_link() -> any().
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Spawn tenmporary process to handle specific request.
%% Because prepare and transfer componrnts need more time to
%% process, so spawn another process to handle those request.
assign_temp_task() ->
	knk_temp_sup:assign_task().


%% %% Direct traffic according to message pattern map
%% %% The map is used to match data, then take operations 
%% %% according to pre-defined ways. In general,
%% %% traffic can be directed to socket or nodes.
%% -spec direct_self_traffic(term()) -> 'ok'.
%% direct_self_traffic(Data) ->
%% 	case whereis(?SERVER) of
%% 		_Pid ->
%% 			?SERVER ! Data;
%% 		undefined ->
%% 			throw("traffic handler not started")
%% 	end,
%% 	ok.

%% Send component distribution request to all system nodes in
%% the network. 
-spec request_distribute(atom(), tuple(), atom()) -> 'ok'.
request_distribute(NodeName, NodeIP, AppName) ->
	gen_server:call(?SERVER, [request_distribute, [request, NodeName,
												   NodeIP, AppName]]).

%% Handle distribute request from outside world. The distribute
%% request is sent and received from knk system socket.
handle_distribute_request(RequestorNode, RequestorIP, AppName) ->
	gen_server:call(?MODULE, [handle_distribute, RequestorNode, 
							  RequestorIP, AppName]).

%% Handle distribution result. The result is success or error.
-spec handle_distribute_success(atom(), list()) -> 'ok'.
handle_distribute_success(RNode, CompInfo) ->
	gen_server:cast(?SERVER, {distribute_sucess, RNode, CompInfo}).

-spec handle_distribute_error(atom(), tuple(), atom()) -> 'ok'.
handle_distribute_error(RNode, RIP, AppName) ->
	gen_server:cast(?SERVER, {distribute_error, RNode, RIP, AppName}).

%% Handle component register request
-spec register_comp(atom(), atom(), atom(), list(), list()) -> 'ok'.
register_comp(CompNode, AppName, CompName, FunList, CompSrc) ->
	gen_server:call(?SERVER, {register_comp, CompNode, AppName, 
							  CompName, FunList, CompSrc}).

%% Handle component register success
-spec register_success(atom(), atom()) -> 'ok'.
register_success(AppName, CompName) ->
	gen_server:call(?SERVER, {register_success, AppName, CompName}).

%% Handle component register error
-spec register_error(atom(), atom(), any()) -> 'ok'.
register_error(AppName, CompName, Reason) ->
	gen_server:call(?SERVER, {register_error, AppName, CompName, Reason}).

%% Handle new components when detecting new components while registering
%% component, this is used as the final approvement for new components. 
-spec handle_new_comp(atom(), atom()) -> 'ok'.
handle_new_comp(AppName, CompName) ->
	gen_server:call(?SERVER, {new_comp, AppName, CompName}).

%% Handle new member of cluster
handle_cluster_member(MemStatus, RequestorIP, RequestorNode, AppName) ->
	case MemStatus of
		join -> gen_server:call(?SERVER, {join, RequestorIP, RequestorNode, AppName})
	end.

%% Message pattern map
message_map_new(Data) ->
	{DataType, DataContent} = Data,
	case DataType of
		sys ->
			[DataRequest, DataInfo] = DataContent,
			case DataRequest of
				register_comp ->
					handle_register(DataInfo);
				request_distribute ->
					knk_socket:send_bcast({socket, [request_distribute,
													DataInfo]});
				tar_transfered_info ->
					knk_socket:send_ucast({socket, [tar_transfered_info,
													DataInfo]})
			end;
		socket ->
			[DataRequest, _DataInfo] = DataContent,
			case DataRequest of
				request_distribute ->
					handle_distribute_request(DataContent);
				tar_transfered_info ->
					handle_tar_transfered_info(DataContent)
			end
	end.

%% Handle register request
handle_register([StorageNode, CompNode, CompInfo]) ->
	{KNKNode, _, _, _} = knk_srv:check_knk_status(),
	gen_server:call({knk_etssrv, StorageNode}, 
					{register_comp, KNKNode, CompNode, CompInfo}).

%% Redirect distribute request to distribute server
handle_distribute_request(RequestContent) ->
	gen_server:call(knk_distribute_srv, RequestContent).

%% Save transfered tar information in local storage
handle_tar_transfered_info(RequestContent) ->
	gen_server:call(knk_distribute_srv, RequestContent).

message_map(Data) ->
	[DataType, DataContent] = Data,
	case DataType of
		comp_distribute ->
			case DataContent of
				[request, RequestorNode, RequestorIP, AppName] when
				  is_atom(RequestorNode), is_tuple(RequestorIP), is_atom(AppName),
				  RequestorNode =/= node() ->
					handle_distribute_request(RequestorNode, RequestorIP, AppName);
				[success, RequestorNode, CompInfo] when
				  is_atom(RequestorNode), is_list(CompInfo), 
				  RequestorNode =/= node() ->
					handle_distribute_success(RequestorNode, CompInfo);
				[error, RequestorNode, RequestorIP, AppName] when
				  is_atom(RequestorNode), is_tuple(RequestorIP), is_atom(AppName),
				  RequestorNode =/= node() ->
					handle_distribute_error(RequestorNode, RequestorIP, AppName)
			end;
		comp_register ->
			case DataContent of
				[request, CompNode, AppName, CompName, FunList, CompSrc] when
				  is_atom(CompNode), is_atom(AppName), is_atom(CompName),
				  is_list(FunList), is_list(CompSrc) ->
					register_comp(CompNode, AppName, CompName, FunList, CompSrc);
				[sucess, AppName, CompName] when
				  is_atom(AppName), is_atom(CompName) ->
					register_success(AppName, CompName);
				[error, AppName, CompName, Reason] when
				  is_atom(AppName), is_atom(CompName) ->
					register_error(AppName, CompName, Reason)
			end;
		comp_traffic ->
			case DataContent of
				[lookup, LookupInfo] when is_list(LookupInfo) ->
					1
			end;
		knk_nodes ->
			case DataContent of
				[component, new, AppName, CompName] 
				  when is_atom(AppName), is_atom(CompName) ->
					handle_new_comp(AppName, CompName);
				[component, distributed, CompName] when is_atom(CompName) ->
					handle_distributed_comp(CompName);
				[component, existed, CompName] when is_atom(CompName) ->
					handle_existed_comp(CompName);
				[component, unwanted, CompName] when is_atom(CompName) ->
					handle_unwanted_comp(CompName);
				[system_status, up, SysInfo] when is_list(SysInfo) ->
					handle_sys_up(SysInfo);
				[system_status, down, SysInfo] when is_list(SysInfo) ->
					handle_sys_down(SysInfo);
				[join, RequestorIP, RequestorNode, AppName] when
				  is_tuple(RequestorIP), is_atom(RequestorNode), is_atom(AppName) ->
					handle_cluster_member(join, RequestorIP, RequestorNode, AppName)
			end
	end.

%% Request distribute
handle_call([comp_distribute, [request, NodeName, NodeIP, AppName]],
			_From, #state{}) ->
	%% send Request to socket, and broadcast
	{reply, ok, #state{}};

%% Start distribution process
handle_call([handle_distribute, RNode, RIP, AppName], _From, #state{}) ->
	{_, SysStatus} = knk_srv:check_status(),
	case SysStatus of
		new -> {noreply, #state{}};
		_Other ->
			Result = knk_distribute:distribute(RNode, RIP, AppName),
			{reply, Result, #state{}}
	end;

handle_call({register_comp, CompNode, AppName, CompName, FunList, CompSrc},
			_From, #state{}) ->
	knk_comp:register_comp([CompNode, AppName, CompName, FunList, CompSrc]),
	{reply, ok, #state{}};

handle_call({register_success, AppName, CompName}, _From, #state{}) ->
	knk_distribute:update_count(AppName, CompName),
	{reply, ok, #state{}};

handle_call({register_error, AppName, CompName, Reason}, _From, #state{}) ->
	{reply, {register_error, AppName, CompName, Reason}, #state{}};

handle_call({new_comp, AppName, CompName}, _From, #state{}) ->
	{reply, knk_comp:handle_new(AppName, CompName), #state{}};

handle_call({join, RequestorIP, RequestorNode, AppName}, _From, #state) ->
	{reply, }.

handle_cast({distribute_success, _RNode, CompInfo}, #state{}) ->
	knk_distribute:update_count(CompInfo),
	{noreply, #state{}};

handle_cast({distribute_error, RNode, RIP, AppName}, #state{}) ->
	knk_distribute:distribute(RNode, RIP, AppName),
	{noreply, #state{}}.

%% ----------------------------------------------------------------------------
%% Handling System Interface API

handle_info([request_distribute, Node, LAddr, AppName], #state{}) ->
	
	{noreply, #state{}}.



init([]) ->
	{ok, #state{}}.

