%% @author billcyz
%% @doc @todo Add description to test_01.


-module(test_01).

%% ====================================================================
%% API functions
%% ====================================================================
%% -export([new_cache_tab/3, new_cache_table/2]).
-compile(export_all).


start() ->
	ets:new(bill, [public, named_table, bag]),
	ets:insert(bill, {test_data, private, [bill]}).
	%%ets:new(test_data, [named_table, bag, public]).

stop() ->
	ets:delete(bill),
	ok.

%% check user permission
chk_usr_permission(User, Owner, Tab) ->
	[{_, _Permission, AccList}] = ets:lookup(Owner, Tab),
	case find_user(AccList, User) of
		{find_user, _} ->
			grant_access;
		{user_not_find, _} ->
			no_access
	end.

%% add user to access list
add_user(User, Owner, Tab) ->
	[{_, Permission, AccList}] = ets:lookup(Owner, Tab),
	ets:delete(Owner, Tab),
	NewAccList = AccList ++ [User],
	Tab_info = {Tab, Permission, NewAccList},
	ets:insert(Owner, Tab_info),
	ets:lookup(Owner, Tab).

%% find user in access list
find_user([H|T], User) ->
	case H of
		User ->
			%% user exists in user list
			{find_user, User};
		_ ->
			find_user(T, User)
	end;

find_user([], User) ->
	{user_not_find, User}.
	
%% 	
%% 	case Permission of
%% 		private ->
%% 			{ok, private};
%% 		local ->
%% 			{ok, local};
%% 		public ->
%% 			{ok, public}
%% 	end.


%% %% Create ets table
%% new_cache_tab(Tab, Type, Acc) ->
%% 	%% Tab = Table name
%% 	%% Type = Table type
%% 	%% Acc = Access type
%% 	case ets:info(Tab) of
%% 		[_] ->
%% 			{table_exists};
%% 		undefined ->
%% 			Tid = ets:new(Tab, [Type, Acc]),
%% 			{ok, Tid}
%% 	end.
%% 
%% new_cache_table(Tab, Options) ->
%% 	case ets:info(Tab) of
%% 		[_] ->
%% 			{table_exists};
%% 		undefined ->
%% 			Tid = ets:new(Tab, Options),
%% 			{ok, Tid}
%% 	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


