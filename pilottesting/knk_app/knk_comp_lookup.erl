%% @author billcyz
%% @doc @todo Add description to knk_comp_lookup.


-module(knk_comp_lookup).

-export([find_app/1, find_comp_list/1, find_node_list/2, find_func_list/2,
		 find_dup_list/3]).


%% --------------------------------------------------------------------------------

%% Find AppName in local app table
-spec find_app(atom()) -> tuple() | 'ok'.
find_app(AppName) ->
	case ets:lookup(app, AppName) of
		[_] -> ok;
		[] -> {error, app_not_found}
	end.

%% Find all components belonged to specific app in local app table
-spec find_comp_list(atom()) -> list().
find_comp_list(AppName) ->
	try ets:lookup_element(comp, AppName, 2) of
		CompList ->
			case length(CompList) of
				0 -> [];
				_ -> CompList
			end
	catch
		error:Error ->
			{error, Error}
	end.

%% Find all nodes of specific component
-spec find_node_list(atom(), atom()) -> tuple() | list().
find_node_list(AppName, CompName) ->
	case length(CompSpecL = ets:lookup(location, AppName)) of
		0 -> {error, empty_app_location};
		_ ->
			find_node_01(CompSpecL, CompName)
	end.

find_node_01([CompH | CompT], CompName) ->
	case lists:keymember(CompName, 2, [CompH]) of
		true ->
			{_, _, CompNodeL} = CompH,
			CompNodeL;
		false ->
			find_node_01(CompT, CompName)
	end;

find_node_01([], _) -> {error, component_not_found}.

%% Search function list belongs to specific component
-spec find_func_list(atom(), atom()) -> list() | tuple().
find_func_list(AppName, CompName) ->
	case length(AppCompL = ets:lookup(comp_fun, AppName)) of
		0 -> {error, app_not_found};
		_ ->
			find_func_01(AppCompL, CompName)
	end.

find_func_01([FunT_H | FunT_T], CompName) ->
	case lists:keymember(CompName, 2, [FunT_H]) of
		true ->
			{_, _, CompFunL} = FunT_H,
			CompFunL;
		false ->
			find_func_01(FunT_T, CompName)
	end;

find_func_01([], _CompName) -> {error, empty_comp_function}.
	
%% Find duplicated information
-spec find_dup_list(list(), list(), list()) -> list().
find_dup_list([NewH | NewT], TargetL, DupL) ->
	case lists:member(NewH, is_list(TargetL)) of
		true ->
			%% io:format("Node ~p duplicated...~n", [NewH]),
			find_dup_list(NewT, TargetL, DupL ++ [NewH]);
		false ->
			find_dup_list(NewT, TargetL, DupL)
	end;

find_dup_list([], _TargetL, DupL) ->
	is_list(DupL).


%% KNK holds multiple applications
%% %% Search all apps within self node, returns app list
%% -spec lookup_localapp() -> tuple() | list().
%% lookup_localapp() ->
%% 	lookup_localapp(app, []).
%% 
%% lookup_localapp(Tab, RList) ->
%% 	case ets:first(Tab) of
%% 		'$end_of_table' -> {error, empty_self_app};
%% 		_ ->
%% 			lookup_localapp(Tab, ets:first(Tab), RList ++
%% 							   ets:first(Tab))
%% 	end.
%% 
%% lookup_localapp(Tab, PrivEle, RList) ->
%% 	case ets:next(Tab, PrivEle) of
%% 		'$end_of_table' -> RList;
%% 		_ ->
%% 			lookup_localapp(Tab, ets:next(Tab, PrivEle), 
%% 						   RList ++ [ets:next(Tab, PrivEle)])
%% 	end.
%%
%% %% Search component according to app name and return organized list
%% -spec lookup_localappcomp(list()) -> list().
%% lookup_localappcomp(LocalAppL) ->
%% 	lookup_localappcomp(LocalAppL, []).
%% 
%% lookup_localappcomp([H | T], RList) ->
%% 	try
%% 		case ets:lookup(self_comp, H) of
%% 			R ->
%% 				lookup_localappcomp(T, RList ++ R)
%% 		end
%% 	catch
%% 		error:Error -> {error, Error}
%% 	end;
%% 
%% lookup_localappcomp([], RList) -> RList.


