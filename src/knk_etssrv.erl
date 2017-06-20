%% @author billcyz
%% @doc @todo Add description to knk_etssrv.


-module(knk_etssrv).
-behaviour(gen_server).
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([check_app/2, 
		 check_comp/3, 
		 get_all_comps/2, 
		 find_wanted_comp/3, 
		 add_comp/4,
		 add_dist_record/3]).

-define(SERVER, ?MODULE).

-record(state, {knk,
				app,
				root_dir}).

%% -----------------------------------------------------------------------------------------

%% -----------------------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------------------

%% Check application name existence in storage
check_app(KNKName, AppName) ->
	gen_server:call(?SERVER, {KNKName, check_app, AppName}).

%% Check component existence in storage
check_comp(KNKName, AppName, CompName) ->
	gen_server:call(?SERVER, {KNKName, check_comp, AppName, CompName}).

%% Get all components belonged to application
get_all_comps(KNKName, AppName) ->
	gen_server:call(?SERVER, {KNKName, get_all_comps, AppName}).

%% Find wanted components for components communication
find_wanted_comp(KNKName, AppName, FunAtts) ->
	gen_server:call(?SERVER, {KNKName, find_wanted_comp, AppName, FunAtts}).

%% Add component info
add_comp(KNKName, AppName, CompName, CompInfo) ->
	gen_server:call(?SERVER, {KNKName, add_comp_info, AppName, CompName, CompInfo}).

%% Add new component into distribution count table
add_dist_record(KNKName, AppName, CompName) ->
	gen_server:call(?SERVER, {KNKName, add_dist_record, AppName, CompName}).

%% Update distribution count of components
update_dist_count(KNKName, AppName, CompName) ->
	gen_server:call(?SERVER, {KNKName, update_dist_count, AppName, CompName}).

%% Register component
register_comp(KNKName, AppName, CompName, CompInfo) ->
	gen_server:call(?SERVER, {KNKName, register_comp, AppName, CompName, CompInfo}).

%% Get component list which should be distributed according to sorted
%% distribute count table
wanted_comp(KNKName, AppName, RANGE) ->
	gen_server:call(?SERVER, {KNKName, AppName, RANGE, wanted_comp}).

%% Check available components running in local knk system
check_available_comp(CompL) ->
	gen_server:call(?SERVER, {check_available_comp, CompL}).

%% Organize information of chosen components which should be distributed
prepare_compinfo(CompL) ->
	gen_server:call(?SERVER, {prepare_compinfo, CompL}).

%% Store compressed file (tar file) hash
store_tarhash(Hash, NodeName, StorePath) ->
	gen_server:call(?SERVER, {store_tarhash, Hash, NodeName, StorePath}).

%% Save ets table to disk
save_table(Tab) ->
	gen_server:call(?SERVER, {save_table, Tab}).

%% Get root directory of knk system
root_dir() ->
	gen_server:call(?SERVER, root_dir).

%% Save tar information into selection_pool table
save_tarinfo() ->
	gen_server:call(?SERVER, 1).

%% Get tar file list from selection pool table
get_tarlist() ->
	gen_server:call(?SERVER, get_tarlist).

%% Get tar hash value
get_tarhash(NodeName, TarName) ->
	gen_server:call(?SERVER, {get_tarhash, NodeName, TarName}).

%% Delete all tar related information in tarhash and selection_pool table 
delete_tar_info() ->
	gen_server:call(?SERVER, delete_tar_info).
	
%% -----------------------------------------------------------------------------------------

%% Starts ets server
start_link(KNKName, AppName) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [KNKName, AppName], []).

init([KNKName, AppName]) ->
    {ok, #state{knk = KNKName, app = AppName}}.

handle_call(check_knk, _From, #state{knk = KNKNode} = State) ->
	{reply, {ok, KNKNode}, State};
handle_call({KNKName, check_app, AppName}, _From, #state{knk = KNK} = State) ->
	if
		KNKName =:= KNK ->
			Result = knk_comp:find_app(AppName),
			{reply, Result, State};
		true ->
			{noreply, State}
	end;
handle_call({KNKName, check_comp, AppName, CompName}, _From, 
			#state{knk = KNK, app = App} = State) ->
	if
		KNKName =:= KNK ->
			if
				AppName =:= App ->
					Result = knk_comp:find_comp(CompName),
					{reply, Result, State};
				true ->
					{reply, {error, wrong_app}, State}
			end;
		true ->
			{noreply, State}
	end;
handle_call({KNKName, get_all_comps, AppName}, _From, #state{knk = KNK,
															 app = App} = State) ->
	if
		KNKName =:= KNK ->
			if
				AppName =:= App ->
					Result = knk_comp:find_all_comps(AppName),
					{reply, Result, State};
				true ->
					{reply, {error, wrong_app}, State}
			end;
		true ->
			{noreply, State}
	end;
handle_call({KNKName, find_wanted_comp, AppName, FunAtts}, _From, 
			#state{knk = KNK, app = App} = State) ->
	if
		KNKName =:= KNK ->
			if
				AppName =:= App ->
					Result = knk_comp:find_comp_by_fun(FunAtts),
					{reply, Result, State};
				true ->
					{reply, {error, wrong_app}, State}
			end;
		true ->
			{noreply, State}
	end;
handle_call({register_comp, KNKNode, CompNode, CompInfo}, _From,
			#state{knk = KNK} = State) ->
	if
		KNKNode =:= KNK ->
			[CompName, FuncList, CompSrc, RunEnv] = CompInfo,
			Result = knk_comp:register_comp(CompName, 
											CompInfo ++ [CompNode]),
			{reply, Result, State};
		true ->
			{reply, {error, wrong_app}, State}
	end;
		true ->
			{noreply, State}
	end;
handle_call({KNKName, add_dist_record, AppName, CompName}, _From,
			#state{knk = KNK, app = App} = State) ->
	if
		KNKName =:= KNK ->
			if
				AppName =:= App ->
					Result = knk_comp:add_dist_record(CompName),
					{result, Result, State};
				true ->
					{reply, {error, wrong_app}, State}
			end;
		true ->
			{noreply, State}
	end;
%% Get components that should be distributed
handle_call({KNKName, AppName, RANGE, wanted_comp}, _From, #state{knk = KNK, 
												  app = App}= State) ->
	if
		KNKName =:= KNK ->
			if
				AppName =:= App ->
					Result = knk_comp:sort_distribute_list(RANGE),
					{reply, Result, State}
			end;
		true ->
			{noreply, State}
	end;
handle_call({check_available_comp, CompL}, _From, State) ->
	Result = knk_comp:find_complist(CompL),
	{reply, Result, State};
handle_call({prepare_compinfo, CompL}, _From, #state{root_dir = RootDir} = State) ->
	case string:sub_string(RootDir, string:len(RootDir)) of
		"/" -> 
			FileList = string:concat(RootDir, "temp_store/file_list.csv"),
			RootNewDir = string:sub_string(RootDir, 1, string:len(RootDir) - 1),
			Result = knk_comp:prepare_comp_csvfile(CompL, FileList),
			{reply, {Result, RootNewDir, FileList}, State};
		_ -> 
			FileList = string:concat(RootDir, "/temp_store/file_list.csv"),
			Result = knk_comp:prepare_comp_csvfile(CompL, FileList),
			{reply, {Result, RootDir, FileList}, State}
	end;
handle_call({store_tarhash, Hash, NodeName, StorePath}, _From, State) ->
	Result = knk_comp:store_tarhash(Hash, NodeName, StorePath),
	{reply, Result, State};
handle_call({save_table, Tab}, _From, State) ->
	Result = knk_comp:save_tab(Tab),
	{reply, Result, State};
handle_call(root_dir, _From, #state{root_dir = RootDir} = State) ->
	case string:sub_string(RootDir, string:len(RootDir)) of
		"/" ->
			RootNewDir = string:sub_string(RootDir, 1, string:len(RootDir) - 1),
			{reply, {ok, RootNewDir}, State};
		_ ->
			{reply, {ok, RootDir}, State}
	end;
handle_call(check_select, _From, State) ->
	Result = ets:first(selection_pool),
	{reply, Result, State};
handle_call(get_tarlist, _From, State) ->
	Result = knk_comp:check_tar_list(),
	{reply, Result, State};
handle_call({get_tarhash, NodeName, TarName}, _From, State) ->
	case knk_comp:get_tarhash(NodeName, TarName) of
		{TarFile, TarHash, StorePath} ->
			{reply, {TarFile, TarHash, StorePath}, State};
		_ ->
			{noreply, State}
	end;
handle_call(delete_tar_info, _From, State) ->
	ets:delete_all_objects(tarhash),
	ets:delete_all_objects(selection_pool),
	{reply, ok, State}.
	







