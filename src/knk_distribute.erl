%% @author billcyz
%% @doc @todo Add description to knk_distribute.

%% Distribution part for KNK system.
%% This works like a temporary process

-module(knk_distribute).
-behaviour(gen_server).

-export([request_distribution/0, check_comp_dist/1]).

%% status is "active" & "running"
-record(state, {knk, app}).

-define(RANGE, 3).
-define(SERVER, ?MODULE).

%% ----------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------

%% Request component distribution
request_distribution() ->
	{_, [NodeName, NodeIP, AppName, _]} = knk_general:basic_info(),
	traffic_handler:message_map({sys, [request_distribute, 
									   [NodeName, NodeIP, AppName]]}).

%% Save transfered tar file information from remote node
save_transfer_info() ->
	gen_server:call(ServerRef, Request).

%% ----------------------------------------------------------------------
%% SERVER
%% ----------------------------------------------------------------------

%% Start distribute server
start_link(KNKName, AppName) ->
	gen_server:start_link(?MODULE, [KNKName, AppName], []).

init([KNKName, AppName]) ->
	{ok, #state{knk = KNKName, app = AppName}}.

handle_call(check_status, _From, #state{status = Status} = State) ->
	{reply, Status, State};
handle_call(check_knk, _From, #state{knk = KNKName} = State) ->
	{reply, {knk_name, KNKName}, State};
handle_call({check_comp_avai, CompName, CompInfo}, _From,
			#state{status = Status} = State) ->
	
	{reply, State};
handle_call([request_distribute, DataInfo], _From, #state{app = App} = State) ->
	%% StorePath should be stored in tarhash table, which can be retrieved by requestor
	[NodeName, NodeIP, User, AppName, StorePath] = DataInfo,
	if
		AppName =:= App ->
			{basic_info, [Node, _, _, SysStatus]} = knk_general:basic_info(),
			if
				SysStatus =:= running andalso NodeName =/= Node ->
					{_, CompNode} = knk_srv:check_comp_node(),
					{_, RootDir} = rpc:call(CompNode, knk_etssrv, root_dir, []),
					{_, KNKName} = knk_srv:check_knk(?SERVER),
					case prepare_distribute(KNKName, CompNode, RootDir, 
											NodeName, StorePath) of
						{ok, TN} -> 
							transfer_tar(NodeName, NodeIP, User, TN, 
										  StorePath, RootDir); 
						no_distribution ->
							{noreply, State}
					end;
				true -> {noreply, State}
			end;
		true ->
			{noreply, State}
	end.

handle_cast({distribute, file_detected}, State) ->
	{_, CompNode} = knk_srv:check_comp_node(),
	{NodeName, NodeTar} = random_tar(CompNode),
	{TarHash, StorePath} = get_tarhash(NodeName, NodeTar),
	{ok, RootDir} = rpc:call(CompNode, knk_etssrv, root_dir, []),
	%% Check tar and file integrity
	Result = os:cmd(lists:flatten(
					  io_lib:format(
						"`which bash` ~p/distribute_transfer.sh check_int ~p ~p ~p",
						[RootDir, NodeTar, TarHash, StorePath]))),
	case Result of
		"tar_checked\n" ->
			1; %% Register component and deploy
		_ ->
			rpc:call(CompNode, knk_etssrv, delete_tarinfo, []),
			rpc:call(CompNode, knk_select_monitor, start_monitor, []),
			2 %% Delete tar file and tar record 
	end,
	1.

%% Prepare distribution process. Sort distribution count table
%% and decide which components should be distributed according
%% to its local storage
prepare_distribute(KNKName, CompNode, RootDir, NodeName, StorePath) ->
	WantedCompL = rpc:call(CompNode, knk_etssrv, wanted_comp, 
						   [KNKName, AppName, ?RANGE]),
	%% Check component existence in component list
	AvaiCompL = rpc:call(CompNode, knk_etssrv, check_available_comp, [WantedCompL]),
	{ok, DistPath} = rpc:call(CompNode, knk_etssrv, save_table, [comp_dist]),
	case rpc:call(CompNode, knk_etssrv, prepare_compinfo, [AvaiCompL]) of
		{ok, RootDir, FileList} -> 
			TarHash = os:cmd(lists:flatten(
							   io_lib:format(
								 "`which bash` ~p/distribute_transfer.sh prepare ~p ~p ~p ~p", 
								 [RootDir, node(), FileList, RootDir, DistPath]))),
			rpc:call(CompNode, knk_etssrv, store_tarhash, [TarHash, NodeName, StorePath]);
		{no_file_to_transfer, _, _} ->
			no_distribution
	end,
	
	1.

%% Transfer tar file to destination. It requires remote node name,
%% ip address of remote node, system user of remote node, tar file that
%% need to be transfered, and directory to store tar file
transfer_tar(RNode, RNodeIP, User, TargetTar, StorePath, RootDir) ->
%% 	[_, Host] = string:tokens(atom_to_list(RNode), "@"),
%% 	DstHost = list_to_atom(Host),
	Result = os:cmd(lists:flatten(
					  io_lib:format(
						"`which bash` ~p/temp_process/distribute_transfer.sh transfer ~p ~p ~p ~p ~p", 
						[RootDir, User, RNodeIP, TargetTar, RootDir, StorePath]))),
	case Result of
		"ok\n" ->
			reply_transfer_info(RNodeIP, RNode, TargetTar, StorePath);
		"tar_not_found\n" ->
			tar_not_found
	end.

%% Send file transfered notification to remote node
%% Send message to the process which monitors selection_pool table
reply_transfer_info(RNodeIP, RNode, TarName, StorePath) ->
	traffic_handler:message_map({sys, [tar_transfered_info, 
									   [node(), RNodeIP, RNode, TarName, 
										StorePath]]}),
	ok.

%% Randomly select component tar file from selection pool table
-spec random_tar(atom()) -> {atom(), list()}.
random_tar(CompNode) ->
	TarList = rpc:call(CompNode, knk_etssrv, get_tarlist, []),
	RandNum = rand:uniform(length(TarList)),
	lists:nth(RandNum, TarList).

%% Get hash value of tar from source node
get_tarhash(NodeName, TarName) ->
	{_, TarHash, StorePath} = rpc:call(NodeName, knk_etssrv, 
							get_tarhash, [node(), TarName]),
	{TarHash, StorePath}.



%% Check components availability. Create temporary node for testing basic
%% functions on components. If the result is "ok", then the component is ready
%% to be used. Otherwise the component can't be used and registerd
check_comp_avai(CompName, CompInfo) ->
	gen_server:call(?SERVER, {check_comp_avai, CompName, CompInfo}).

%% Get process status
check_status() ->
	gen_server:call(?SERVER, check_status).

%% Initialize distribution part when the system start
distribute_init() ->
	ok = create_count_tab(),
	case string:str(
		   os:cmd("nc -z -v 127.0.0.1 22"), "succeeded") of
		0 -> {error, ssh_port_unavailable};
		_ ->
			check_extra_service([rsync, ssh, md5sum, tar])
	end.

%% Check required service on sevrer, such as SSH and md5sum
check_extra_service(ServiceL) ->
	ExtServL = [os:cmd(lists:flatten(io_lib:format("which ~p", [X]))) 
		|| X <- ServiceL],
	check_empty_service(ServiceL, ExtServL, []).

check_empty_service([ServH | ServT], [H | T], E) ->
	if
		H =:= [] -> check_empty_service(ServT, T, 
										E ++ [err_result(ServH)]);
		H =/= [] -> check_empty_service(ServT, T, E)
	end;
check_empty_service([], [], E) ->
	if
		length(E) =/= 0 -> E;
		true -> ok
	end.

err_result(ServH) ->
	{error, lists:concat([ServH, "_", unavailable])}.

%% Create distribute count table
%% [dist_count, AppName, {CompName, Count}]
-spec create_count_tab() -> 'ok' | tuple().
create_count_tab() ->
	ets:new(dist_count, [bag, private, named_table]),
	case ets:info(dist_count) of
		undefined -> {error, count_tab_failed};
		_ -> ok
	end.

%% Adding new record into distribute count table
add_dist_count(CompName) ->
	CompDistL = ets:first(comp_dist),
	ets:delete_object(comp_dist, CompDistL),
	ets:insert(comp_dist, {CompDistL ++ [{CompName, 1}]}),
	ok.

%% Update count table
%%%% Should consider receiving unknown components %%%%
-spec update_count(atom()) -> tuple().
update_count(CompName) ->
	CompDistL = ets:first(comp_dist),
	ets:delete_object(comp_dist, CompDistL),
	lists:map(
	  fun(CompD) ->
			  {CN, CNum} = CompD,
			  if
				  CN =:= CompName ->
					  , CompDistL),
	ok.
	
	lists:map(
	  fun(AppName) -> 
			  [{_, Count}] = ets:lookup_element(dist_count, AppName, 2), 
			  ets:insert(dist_count, {CompName, Count + 1}) end, 
	  CompL),
	ok.

%% Distribute component to destination
-spec distribute([node()], tuple(), atom()) -> 'ok'.
distribute(RNode, RIP, AppName) ->
	CompCandidateL = comp_candidate(AppName, ?RANGE),
	CompL = [CN || {CN, _} <- CompCandidateL],
	%% check component existence
	check_comp_exist(AppName, CompL),

%% Generate hash value for components that going to be
%% distributed
gen_hash() ->
	1.

%% Select components to be distributed. Components with the least numbers 
%% will be distributed. If the components to be distribuetd are not in the 
%% storage, then ignore the message and send no reply.
%% [{comp_01, 2}, {comp_02, 3}]  
-spec comp_candidate(atom(), integer()) -> list().
comp_candidate(AppName, Range) ->
	lists:sublist(lists:keysort(2, ets:lookup_element(dist_count, AppName, 2)),
				  Range).

%% Check component existence in "app_dir" directory
%% CompL = [comp_01, comp_02]
check_comp_exist(AppName, CompL) ->
	AppDir = knk_app:get_config(app_dir),
	%% check component source on storage node
	1.





