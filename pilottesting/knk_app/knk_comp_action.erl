%% @author billcyz
%% @doc @todo Add description to knk_comp_action.


-module(knk_comp_action).

-export([add_app_info/1, 
		 add_comp_info/2,
		 add_location_info/3,
		 add_comp_func/3,
		 replace_comp_node/3, replace_comp_fun/3,
		 delete_comp/2]).


%% ------------------------------------------------------------------------------------
%% Add

%% Add application name in local application storage
-spec add_app_info(atom()) -> tuple().
add_app_info(AppName) ->
	case ets:lookup(app, AppName) of
		[] -> 
			true = ets:insert(app, {AppName}),
			ok;
		[_] ->
			{info, app_existed}
	end.

%% Add component name in local cmponent storage
-spec add_comp_info(atom(), atom()) -> any().
add_comp_info(AppName, CompName) ->
	case knk_comp_lookup:find_comp_list(AppName) of
		[] -> 
			true = ets:insert(comp, {AppName, [CompName]}),
			ok;
		CL when is_list(CL) ->
			case lists:member(CompName, CL) of
				true -> {info, component_existed};
				false -> 
					ets:insert(comp, {AppName, CL ++ [CompName]}),
					ok
			end;
		E -> E
	end.

-spec add_location_info(atom(), atom(), [node()]) -> tuple() | 'ok'.
add_location_info(AppName, CompName, CompNode) ->
	case knk_comp_lookup:find_node_list(AppName, CompName) of
		NodeList when is_list(NodeList) ->
			NodeDupList = knk_comp_lookup:find_dup_list(CompNode, NodeList, []),
			if
				length(CompNode) == length(NodeDupList) ->
					{error, all_node_duplicated};
				true ->
					ets:delete_object(location, {AppName,
												 CompName, NodeList}),
					ets:insert(location, {AppName, CompName,
										  NodeList ++ (CompNode -- NodeDupList)}),
					ok
			end;
		E -> E
	end.

%% Add functions within components
-spec add_comp_func(atom(), atom(), [tuple()]) -> tuple() | 'ok'.
add_comp_func(AppName, CompName, Func) ->
	case knk_comp_lookup:find_func_list(AppName, CompName) of
		FunList when is_list(FunList) ->
			FunDupList = knk_comp_lookup:find_func_list(Func, FunList, []),
			if
				length(Func) == length(FunDupList) ->
					{error, all_function_duplicated};
				true ->
					ets:delete_object(comp_fun, {AppName,
												  CompName, FunList}),
					ets:insert(comp_fun, {AppName, CompName,
										   FunList ++ (Func -- FunDupList)}),
					ok
			end;
		E -> E
	end.


%% Update


%% Replace

-spec replace_comp_node(atom(), atom(), [node()]) -> 'ok'.
replace_comp_node(AppName, CompName, CompNodeL) ->
	ets:delete_object(location, {AppName, CompName,
								 knk_comp_lookup:find_comp_list(AppName)}),
	ets:insert(location, {AppName, CompName, CompNodeL}),
	ok.

-spec replace_comp_fun(atom(), atom(), [tuple()]) -> 'ok'.
replace_comp_fun(AppName, CompName, CompFunL) ->
	ets:delete_object(comp_fun, {AppName, CompName,
								 knk_comp_lookup:find_func_list(AppName, CompName)}),
	ets:insert(comp_fun, {AppName, CompName, CompFunL}),
	ok.


%% Delete

%% Delete App
delete_app(AppName) ->
	ets:delete(app, AppName),
	ets:delete(comp, AppName),
	ets:delete(location, AppName),
	ets:delete(comp_fun, AppName),
	ok.

%% When one component is down unexpectively, then the local storage delete all component 
%% information in local storage
-spec delete_comp(atom(), atom()) -> 'ok' | tuple().
delete_comp(AppName, CompName) ->
	CompL = knk_comp_lookup:find_comp_list(AppName),
	NodeL = knk_comp_lookup:find_node_list(AppName, CompName),
	FunL = knk_comp_lookup:find_func_list(AppName, CompName),
	case lists:member(CompName, CompL) of
		true ->
			case length(CompL) of
				1 ->
					delete_app(AppName);
				_ ->
					ets:insert(comp, {AppName, CompL -- [CompName]}),
					ets:delete_object(location, {AppName, CompName, NodeL}),
					ets:delete_object(comp_fun, {AppName, CompName, FunL}),
					ok
			end;
		false ->
			{error, not_available_component}
	end.





