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

%% -----------------------------------------------------------------------

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

%% %% Create app table
%% %% {app, {AppName}}
%% -spec create_app_table() -> 'ok'.
%% create_app_table() ->
%% 	knk_general:create_ets(app, [protected,
%% 								 named_table,
%% 								 {read_concurrency, ture}]),
%% 	ok.
%% 
%% %% Create component table
%% %% {comp, {AppName, [CompName]}}
%% -spec create_comp_table() -> 'ok'.
%% create_comp_table() ->
%% 	knk_general:create_ets(comp, [protected,
%% 								  named_table,
%% 								  {read_concurrency, true}]),
%% 	ok.
%% 
%% %% Create location table
%% %% {location, {AppName, CompName, [NodeName]}}
%% -spec create_location_table() -> 'ok'.
%% create_location_table() ->
%% 	knk_general:create_ets(location, [bag, protected,
%% 									  named_table,
%% 									  {read_concurrency, true}]),
%% 	ok.
%% 
%% %% Create function table
%% %% {comp_fun, {AppName, CompName, [{fun_01, 1}, {fun_02, 2}]}}
%% -spec create_func_table() -> 'ok'.
%% create_func_table() ->
%% 	knk_general:create_ets(comp_func, [bag, protected,
%% 									   named_table,
%% 									   {read_concurrency, true}]),
%% 	ok.
%% 
%% %% Create source code table. Store the compiled file name of the component
%% %% {comp_src, {AppName, CompName, [CompSrc]}}
%% create_compsrc_table() ->
%% 	knk_general:create_ets(comp_src, [protected,
%% 									  named_table,
%% 									  {read_concurrency, true}]),
%% 	ok.
%%
%% Check component data existence (app/comp). The component data is shared 
%% with all nodes.
%% -spec check_appcomp_tab(atom()) -> {notify, undefined_comp} | 'ok'.
%% check_appcomp_tab(Tab) ->
%% 	case knk_general:check_ets(Tab) of
%% 		{tab_notexist} -> 
%% 			case Tab of
%% 				app -> {notify, undefined_app};
%% 				comp -> {notify, undefined_comp}
%% 			end;
%% 		{tab_exist} -> ok
%% 	end.
%%
%% Check component storage file, and store data into the component storage
%% read_comp_file(File) ->
%% 	case file:open(File, [read]) of
%% 		{ok, _} ->
%% 			io:format("~s is found...~n", [File]); %% send to log file
%% 		{error, enoent} ->
%% 			io:format("~s is missing...~n", [File]);
%% 		{error, Reason} ->
%% 			io:format("~s is ~s~n", [File, Reason])
%% 	end.
%%
%% Save tables to file
%% dump_table(Tab, Option) ->
%% 	Tab + Option.


%% --------------------------------------------------------------------------------------------

%% Save ets table to disk
save_tab(Tab) ->
	DataDir = application:get_env(knk, data),
	ets:tab2file(Tab, lists:concat([DataDir, "/", Tab, ".edata"])).

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

%% Select components to be distributed. Components with the least three numbers 
%% will be distributed. If the components to be distribuetd are not in the 
%% storage, then ignore the message and send no reply. 
%% [comp_count, {app_01, [{comp_01, 2}, {comp_02, 3}]}]
-spec distribute_list(atom(), any()) -> list().
distribute_list(AppName, Count) ->
	lists:sublist(
				  lists:keysort(2, ets:lookup_element(comp_count, AppName, 2)), 
				  Count).

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

%% new_comp_count(AppName, [AppCompH | AppCompT], [{CompName, CompCount} | CompT], R) ->
%% 	if
%% 		AppCompH == CompName ->
%% 			new_comp_count(AppName, AppCompT, CompT, R);
%% 		AppCompH /= CompName ->
%% 			new_comp_count(AppName, [AppCompH | AppCompT], CompT, 
%% 						   R ++ [{CompName, CompCount}])
%% 	end;
%% 
%% new_comp_count(_, [_], [], _) -> {error, duplicated_comp};
%% 
%% new_comp_count(AppName, [], [_], R) ->
%% 	ets:insert(comp_count, {AppName, R}).

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


%% -------------------------------------------------------------------------------------------

%% find_comp(CompList, CompName) ->
%% 	find_comp(CompList, CompName, []).
%% 
%% find_comp(CompList, CompName, RList) ->
%% 	[CompH | CompT] = CompList,
%% 	case [CompH] == CompName of
%% 		true -> {notify, comp_existed};
%% 		false ->
%% 			find_comp(CompT, CompName, RList ++ [CompH])
%% 	end;
%% 
%% find_comp([], _CompName, RList) -> RList.


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






