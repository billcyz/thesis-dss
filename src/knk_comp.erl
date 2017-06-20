%% @author billcyz
%% @doc @todo Add description to knk_comp.

%% Component id, node name, comp interface
%% Component storage. It stores the component unique id, node information,
%% and component interface for communication. The storage data will be the 
%% same on all nodes in the system, and can update at the same time.
%% 
%% The component storage can be stored in files or cache. 

-module(knk_comp).

-export([ets_table_init/0, ets_fileinitialize/1,
		 ets_check/0,
		 check_comp_exi/2, check_node_exi/3, check_fun_exi/3,
		 register_comp/1, update_count/3,
		 distribute_list/2,
		 is_new_srv/1]).

-export([create_tab/1, find_complist/1, identify_comp/2, sort_distribute_list/1,
		 prepare_comp_csvfile/2, store_tarhash/3, save_tab/1, check_tar_list/0,
		 get_tarhash/2]).

%% -----------------------------------------------------------------------

%% Create table according to the name of table
%%
%% Table name: app
%% Table sturcture: {app, AppName}
%% Table type: set
%% 
%% Table name: comp
%% Table sturcture: {comp, [CompName]}
%% Table type: set
%% 
%% Table name: comp_atts
%% Table sturcture: {comp_atts, CompName, CompNode, [{FunName, ArgNum}], CompSrc, RunEnv}
%% Table type: set
%% 
%% Table name: comp_dist
%% Table sturcture: {comp_dist, {CompName, CountNumber}}
%% Table type: set
%%
%% Table name: tarhash
%% Table structure: {tarhash, {NodeName, TarFile, HashValue}}
%% Table type: set
%%
%% Table name: selection_pool
%% Table structure: {selection_pool, {NodeName, TarFile}}
%% Table type: set
create_tab(TabList) ->
	lists:map(
	  fun(Tab) -> ets:new(Tab, [named_table,
								{heir, self(), []}]) end, TabList),
	SRV = whereis(knk_etssrv),
	lists:map(
	  fun(Tab) -> ets:give_away(Tab, SRV, []) end, TabList).

%% Find AppName in local app table
-spec find_app(atom()) -> atom().
find_app(AppName) ->
	case ets:lookup(app, AppName) of
		[_] -> app_exist;
		[] -> app_not_found
	end.

%% Find CompName in local comp table
-spec find_comp(atom()) -> atom().
find_comp(CompName) ->
	CompL = find_all_comps(),
	case lists:member(CompName, CompL) of
		true ->
			comp_exist;
		false ->
			comp_not_found
	end.

%% Find all components in local comp table
-spec find_all_comps() -> list().
find_all_comps() ->
	ets:first(comp).

%% Add component information. must be called by register
-spec add_comp_info(atom(), [tuple()]) -> 'ok'.
add_comp_info(CompName, CompInfo) ->
	CompL = find_all_comps(),
	NCompL = case lists:member(CompName, CompL) of
				 true -> CompL;
				 false -> CompL ++ [CompName]
			 end,
	ets:insert(comp, NCompL),
	ets:insert(comp_atts, CompInfo).
	
%% Find component information (attributes)
find_comp_atts(CompName) ->
	case ets:lookup(comp_atts, CompName) of
		[] -> 
			comp_not_found;
		CompInfo when is_list(CompInfo) ->
			CompInfo
	end.

%% Find component list existence in local storage. Components
%% must exist in "comp" table and "comp_atts" table
find_complist(CompL) ->
	AllCompL = find_all_comps(),
	find_complist_l(CompL, AllCompL, []).

find_complist_l([CompH | CompT], AllCompL, RList) ->
	case lists:member(CompH, AllCompL) of
		true ->
			case find_comp_atts(CompH) of
				comp_not_found ->
					find_complist_l(CompT, AllCompL, RList);
				[_] ->
					find_complist_l(CompT, AllCompL, RList ++
										[CompH])
			end;
		false ->
			find_complist_l(CompT, AllCompL, RList)
	end;
find_complist_l([], _, RList) -> RList.

%% Check component distribute count table
-spec check_comp_dist(atom()) -> list() | atom().
check_comp_dist(CompName) ->
	case ets:lookup(comp_dist, CompName) of
		[CompDist] when is_tuple(CompDist) ->
			[CompDist];
		[] ->
			case ets:first(comp_dist) of
				'$end_of_table' ->
					empty_dist_table;
				_Other ->
					comp_dist_not_found
			end
	end.

%% Get full list of distribution count
%% {comp_dist, {CompName, CountNumber}}
%% [{CompName, CompCount}]
full_distribute_list() ->
	case ets:first(comp_dist) of
		'$end_of_table' -> empty_dist_count;
		HComp ->
			full_distribute_list(HComp, [])
	end.

full_distribute_list(HComp, RList) ->
	HCompCountL = ets:lookup(comp_dist, HComp),
	case ets:next(comp_dist, HComp) of
		'$end_of_table' -> RList ++ HCompCountL;
		NComp ->
			full_distribute_list(NComp, RList ++ HCompCountL)
	end.

%% Select components to be distributed. Components with the least three numbers 
%% will be distributed. If the components to be distribuetd are not in the 
%% storage, then ignore the message and send no reply. 
%% {comp_dist, {CompName, CountNumber}}
sort_distribute_list(RANGE) ->
	case full_distribute_list() of
		DistL when is_list(DistL) ->
			[X || {X, _} <- lists:sublist(lists:keysort(2, DistL), RANGE)];
		empty_dist_count -> no_count_list
	end.

%% Identify types for incoming components 
%% Components can be divided into three types. 
-spec identify_comp(atom(), [tuple()]) -> any().
identify_comp(CompName, CompInfo) ->
	case find_comp(CompName) of
		comp_exit ->
			case find_comp_atts(CompName) of
				CompAtts ->
					case check_comp_dist(CompName) of
						empty_dist_table -> dist_table_error;
						comp_dist_not_found -> comp_error;
						_Other ->
							if
								CompAtts =:= CompInfo ->
									duplicated_comp;
								true ->
									new_vsn_comp
							end
					end;
				[] -> comp_error
			end;
		comp_not_found ->
			case find_comp_atts(CompName) of
				comp_not_found ->
					case check_comp_dist(CompName) of
						empty_dist_table -> new_sys_comp;
						comp_dist_not_found -> new_sys_comp;
						CompDist when is_list(CompDist) ->
							new_local_comp
					end;
				_ -> comp_error
			end
	end.

%% Register components and update distribute count table
%% CompInfo = {CompName, [{FunName, ArgNum}], CompSrc}
-spec register_comp(atom(), {atom(), [tuple()], list()}) -> 'ok'.
register_comp(CompName, CompInfo) ->
	case knk_distribute:check_comp_avai(CompName, CompInfo) of
		ok -> 1;
		no -> 2
	end,
	
	case identify_comp(CompName, CompInfo) of
		new_sys_comp ->
			add_comp_info(CompName, CompInfo),
			add_dist_record(CompName);
		new_local_comp ->
			2;
		new_vsn_comp ->
			3;
		duplicated_comp -> ignore;
		E -> E
	end.

%% Add component record in distribute count table
-spec add_dist_record(atom()) -> 'ok'.
add_dist_record(CompName) ->
	ets:insert(comp_dist, {CompName, 1}),
	ok.

%% Delete component record in distribute count table
-spec delete_dist_record(atom()) -> 'ok'.
delete_dist_record(CompName) ->
	ets:delete(comp_dist, CompName),
	ok.

%% Update distribute count table
-spec update_count(atom(), atom()) -> 'ok'.
update_count(Type, CompName) ->
	[{_, CompCount}] = ets:lookup(comp_dist, CompName),
	case Type of
		add ->
			ets:insert(comp_dist, {CompName, CompCount + 1});
		minus ->
			if
				CompCount - 1 =< 0 ->
					delete_dist_record(CompName);
				true ->
					ets:insert(comp_dist, {CompName, CompCount - 1})
			end
	end,
	ok.

%% Save ets table to disk
-spec save_tab(atom()) -> 'ok'.
save_tab(Tab) ->
	DataDir = application:get_env(knk, data),
	case string:sub_string(DataDir, string:len(DataDir)) of
		"/" ->
			TabPath = lists:concat([DataDir, Tab, ".edata"]),
			ets:tab2file(Tab, TabPath);
		_ -> 
			TabPath = lists:concat([DataDir, "/", Tab, ".edata"]),
			ets:tab2file(Tab, TabPath)
	end,
	{ok, TabPath}.

%% Store component info into pre-defined csv file
prepare_comp_csvfile([CompH | CompT], FileList) ->
	case ets:lookup(comp_atts, CompH) of
		[] -> prepare_comp_csvfile(CompT, FileList);
		[{CompH, CompPath}] ->
			file:write_file(FileList, lists:flatten(
							  io_lib:format("~p,~p~n", [CompH, list_to_atom(CompPath)])), 
							[append]),
			prepare_comp_csvfile(CompT, FileList)
	end;
prepare_comp_csvfile([], FileList) ->
	case filelib:is_file(FileList) of
		true -> ok;
		false -> no_file_to_transfer
	end.

%% Store tar file hash in "tarhash" table
store_tarhash(Hash, NodeName, StorePath) ->
	[Value, TarFile] = string:tokens(Hash, " \n"),
	ets:insert(tarhash, {NodeName, TarFile, Value, StorePath}),
	{ok, TarFile}.

%% Get tar hash value
get_tarhash(NodeName, TarName) ->
	case ets:lookup(tarhash, NodeName) of
		[] -> no_tar_file;
		[{_, TarFile, TarHash, StorePath}] ->
			if
				TarFile =:= TarName -> {TarFile, TarHash, StorePath};
				true -> wrong_tar_file
			end
	end.

%% Select tar list from selection pool table
check_tar_list() ->
	case ets:first(selection_pool) of
		NodeName ->
			check_tar_list(NodeName, [])
	end.

check_tar_list(NodeName, RList) ->
	TarInfo = ets:lookup(selection_pool, NodeName),
	case ets:next(selection_pool, NodeName) of
		NodeNext ->
			check_tar_list(NodeNext, RList ++ TarInfo);
		'$end_of_table' -> RList ++ TarInfo
	end.


%% ----------------------------------------------------------------------------------------------------
%% Local storage initialization
ets_table_init() ->
	try
		lists:map(
		  fun(Tab) -> ets:new(Tab, [protected, named_table]) end, 
		  [app, comp, comp_src]),
		lists:map(
		  fun(Tab) -> ets:new(Tab, [bag, protected, named_table]) end, 
		  [location, comp_func]),
		knk_distribute:create_dist_table(),
		knk_log:write_log(
		  normal, lists:flatten(io_lib:format("Local component storage initialized...~n", 
														  []))),
		ok
	catch
		error:Error -> {error, Error}
	end.

%% Load storage file if exists, if any file doesn't exist, then an empty
%% local storage is created. The storage file is appets, compets, funcets,
%% and locaets.
ets_fileinitialize(FileList) ->
	knk_log:write_log(normal,
					  lists:flatten(io_lib:format("Loading storage file...~n", 
														  []))),
	case lists:member(false, lists:map(fun(File) ->
						  filelib:is_file(File) end, FileList)) of
		true -> ets_init;
		false ->
			lists:map(fun(File_01) ->
							  ets:file2tab(File_01, [{verify, true}]) end, FileList),
			ok
	end.

%% Search required table existence
%% app, comp, location, comp_fun
ets_check() ->
	case lists:member(undefined, lists:map(fun(Tab) -> ets:info(Tab) end, 
			  [app, comp, location, comp_fun])) of
		true -> {error, tab_not_exist};
		false -> {info, tab_exists}
	end.

%% Check component existence in local component storage
-spec check_comp_exi(atom(), atom()) -> tuple() | 'ok'.
check_comp_exi(AppName, CompName) ->
	CompL = knk_comp_lookup:find_comp_list(AppName),
	case lists:member(CompName, CompL) of
		true -> ok;
		false -> {comp_not_found}
	end.

%% Check node existence in local location storage
-spec check_node_exi(atom(), atom(), node()) -> tuple() | 'ok'.
check_node_exi(AppName, CompName, NodeName) ->
	CompNodeL = knk_comp_lookup:find_node_list(AppName, CompName),
	case lists:member(NodeName, CompNodeL) of
		true -> ok;
		false -> {node_not_found}
	end.
		
%% Check function existence in local function storage
-spec check_fun_exi(atom(), atom(), [tuple()]) -> tuple() | 'ok'.
check_fun_exi(AppName, CompName, FunInfo) ->
	CompFunL = knk_comp_lookup:find_func_list(AppName, CompName),
	case lists:member(FunInfo, CompFunL) of
		true -> ok;
		false -> {func_not_found}
	end.

%% Register component
%% AppName (knk_encryptor), CompName (knk_encryptor_file), NodeName ('node_01@127.0.0.1')
%%
%% Should return message to supervisor or server when register process is done. Each step should 
%% set up time limit, and report error when time exceeds or problems happened in step. Once the 
%% supervisor receives error, all modification should be rolled back to previous state. 
%% If the application name does not match the one stored in local storage, then report error.
register_comp([AppName, CompName, NodeName, FunList]) ->
	case ets:lookup(app, AppName) of
		[] -> {error, wrong_app_name};
		[_] ->
			case lists:member(CompName, knk_comp_lookup:find_comp_list(AppName)) of
				true -> update_exi_comp([AppName, CompName, NodeName, FunList]);
				false -> register_new_comp([AppName, CompName, NodeName, FunList])
			end
	end.

-spec update_exi_comp(list()) -> 'ok'.
update_exi_comp([AppName, CompName, NodeName, FunList]) ->
	CompNodeL = knk_comp_lookup:find_node_list(AppName, CompName),
	CompFunL = knk_comp_lookup:find_func_list(AppName, CompName),
	case lists:member(NodeName, CompNodeL) of
		false ->
			knk_comp_action:replace_comp_node(AppName, CompName, [NodeName])
	end,
	case FunList == CompFunL of
		false ->
			knk_comp_action:replace_comp_fun(AppName, CompName, FunList)
	end,
	ok.

-spec register_new_comp(list()) -> 'ok'.
register_new_comp([AppName, CompName, NodeName, FunList]) ->
	ok = knk_comp_action:add_comp_info(AppName, CompName),
	ok = knk_comp_action:add_location_info(AppName, CompName, NodeName),
	ok = knk_comp_action:add_comp_func(AppName, CompName, FunList),
	ok.

%% -------------------------------------------------------------------------

%% Update the count of components
%% The action supports add and delete. When a new component is registered, the count table
%% will add the new component, and set the count to 0.
-spec update_count(atom(), atom(), list()) -> 'ok' | tuple().
update_count(Action, AppName, CompL) ->
	AppCompL = ets:lookup(comp_count, AppName, 2),
	case Action of
		add -> add_comp_count(AppName, AppCompL, CompL, []);
		del -> del_comp_count(AppName, AppCompL, CompL, [])
	end,
	ok.

add_comp_count(AppName, [AppCompH | AppCompT], [{CompName, CompCount} | CompT], R) ->
	if
		AppCompH == CompName -> 
			add_comp_count(AppName, AppCompT, CompT, R ++ [{CompName, CompCount + 1}]);
		AppCompH /= CompName ->
			add_comp_count(AppName, [AppCompH | AppCompT], CompT, 
						   R ++ [{CompName, CompCount}])
	end;

add_comp_count(_, [_], [], _R) -> {error, comp_not_found};

add_comp_count(AppName, [], [_], R) ->
	ets:insert(comp_count, {AppName, R}).

del_comp_count(AppName, [AppCompH | AppCompT], [{CompName, CompCount} | CompT], R) ->
	if
		AppCompH == CompName ->
			if 
				CompCount - 1 > 0 ->
					del_comp_count(AppName, AppCompT, CompT, 
								   R ++ [{CompName, CompCount - 1}]);
				CompCount -1 =< 0 ->
					knk_comp_action:delete_comp(AppName, CompName)
			end;
		AppCompH /= CompName ->
			del_comp_count(AppName, [AppCompH | AppCompT], CompT, 
						   R ++ [{CompName, CompCount}])
	end;

del_comp_count(_, [_], [], _R) -> {error, comp_not_found};

del_comp_count(AppName, [], [_], R) ->
	ets:insert(comp_count, {AppName, R}).

%% Get node cookie
-spec get_cookie(atom(), atom(), atom()) -> atom() | atom().
get_cookie(AppName, CompName, NodeName) ->
	CompList = ets:lookup(app, AppName),
	case find_comp_tuple(CompList, CompName, NodeName) of
		Tuple ->
			{_, _, _, Cookie} = Tuple,
			DCookie = decrypt(Cookie), %% decrypt cookie into plain text
			{ok, DCookie};
		{error, E} ->
			E
	end.

find_comp_tuple(CompList, CompName, NodeName) ->
	case lists:keyfind(CompName, 2, CompList) of
		false ->
			{error, comp_not_found};
		Tuple ->
			case lists:keyfind(NodeName, 3, Tuple) of
				false ->
					{error, node_not_found};
				CTuple ->
					CTuple
			end
	end.

%% Check component connection
-spec test_comp_conn(atom(), atom()) -> 'ok' | {error, node_not_connected}.
test_comp_conn(NodeName, Cookie) ->
	case net_adm:ping(NodeName) of
		pong -> ok;
		pang -> {error, node_not_connected}
	end.

%% Start connection node
%% Each component belongs to one applicaiton should use the same cookie,
%% and the cookie is used in knk to create communication node for components,
%% so the node becomes one part of the application.
start_conn_node(AppName, Cookie) ->
	Pid = spawn(?MODULE, start_node, [AppName, Cookie]),
	os:cmd("`which bash` knksys.sh create_node Cookie").
	

%% Check whether the component storage is empty or not. If the storage
%% is empty, then it is a new server. If the storage is not empty, then it
%% is already been in the knk cluster.
-spec is_new_srv(list()) -> boolean().
is_new_srv(TabList) ->
	case find_not_empty(
		   lists:map(fun(Tab) -> ets:info(Tab, size) end, TabList)) of
		not_new -> false;
		new -> true
	end.

find_not_empty([H | T]) ->
	if
		H =/= 0 -> not_new;
		H =:= 0 -> find_not_empty(T)
	end;
find_not_empty([]) -> new.






