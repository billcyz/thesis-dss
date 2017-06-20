%% @author billcyz
%% @doc @todo Add description to test_02.


-module(test_02).

%% ====================================================================
%% API functions
%% ====================================================================
%%-export([]).

-compile(export_all).

check(L) ->
	if
		L == [] -> {is_empty};
		L /= [] -> {not_empty}
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% create_comp_ets() ->
%% 	ets:new(comp, [named_table]),
%% 	ok.
%% 
%% insert_comp_ets(AppName) ->
%% 	ets:insert(comp, {AppName, [comp_01, comp_02]}),
%% 	ok.
%% 
%% %% Search component list
%% -spec lookup_comp(atom()) -> list().
%% lookup_comp(AppName) ->
%% 	%%try ets:lookup_element(comp, AppName, 2) of
%% 	try ets:lookup_element(comp, AppName, 2) of
%% 		CompList ->
%% 			case erlang:length(CompList) of
%% 				0 -> [];
%% 				_ -> CompList
%% 			end
%% 	catch
%% 		error:Error ->
%% 			{error, Error}
%% 	end.





%% Find AppName in local app table
-spec find_app(atom()) -> tuple() | 'ok'.
find_app(AppName) ->
	try
		case ets:lookup(app, AppName) of
			[_] -> ok;
			[] -> {info, app_not_found}
		end
	catch
		error:Error -> Error
	end.

one() ->
	A = [1,2,3,4,5],
	B = [1,2,3,4,5],
	case A /= B of
		false -> {info, same}
	end.

two() ->
	A = [1,2,3],
	A.

select_comp(SortedL) ->
	F = fun(_, R) ->
				select_comp_01(R, R, SortedL)
		end,
	F_01 = 
	lists:foldl(F, [], lists:seq(1, 3)).

select_comp_01(_R, R, [SortH | SortT]) ->
	select_comp_01(_R, R ++ [SortH], SortT);

select_comp_01(_R, R, []) -> R.
