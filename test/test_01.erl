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

%% check user Acctype
chk_usr_Acctype(User, Owner, Tab) ->
	[{_, _Acctype, AccList}] = ets:lookup(Owner, Tab),
	case find_user(AccList, User) of
		{find_user, _} -> {grant_access};
		{find_anonymous, _} -> {grant_access};
		{user_not_find, _} -> {no_access}
	end.

%% update (add/delete) user to access list
update_user(User, Owner, Tab, Action) ->
	[{_, Acctype, AccList}] = ets:lookup(Owner, Tab),
	ets:delete(Owner, Tab),
	case Action of
		add -> 
			NewAccList = AccList ++ [User],
			Tab_info = {Tab, Acctype, NewAccList};
		delete ->
			NewAccList = AccList -- [User],
			if
				NewAccList == [] -> Tab_info = {Tab, Acctype, [Owner]};
 				NewAccList /= [] ->	Tab_info = {Tab, Acctype, NewAccList}
			end
	end,
	ets:insert(Owner, Tab_info),
	ets:lookup(Owner, Tab).
	

%% find user in access list (anonymous)
find_user([H|T], User) ->
	case H of
		User ->
			%% user exists in user list
			{find_user, User};
		anonymous -> 
			{find_anonymous, User};
		_ ->
			find_user(T, User)
	end;

find_user([], User) ->
	{user_not_find, User}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% update data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_cache_tab(User, Tab, Options) ->
	%% check user ets collection
	{User, Msg} = ?MODULE:chk_usr_collection(User),
	case Msg of
		collect_exists ->
			case ets:lookup(User, Tab) of
				[_] ->
					{table_exists};
				[] ->
					{table_not_exists},
					Tid = ets:new(Tab, Options),
					?MODULE:update_usr_collection(User, {Tab, Options}, insert),
					{ok, User, {Tab, Tid}}
			end;
		no_collection ->
			{error, User, collection_not_exists}
	end.







%% update data accroding to user permission
update_data(User, Tab, NewData) ->
	1.

%% update data without user restrictions
update_data(anonymous, Tab, NewData) ->
	2.

new_cache_tab(Tab, Type, Acc) ->
	%% Tab = Table name
	%% Type = Table type
	%% Acc = Access type
	case ets:info(Tab) of
		[_] ->
			{table_exists};
		undefined ->
			Tid = ets:new(Tab, [Type, Acc]),
			{ok, Tid}
	end.

new_cache_tab(Tab, Type, Acc, named_table) ->
	case ets:info(Tab) of
		[_] ->
			{table_exists};
		undefined ->
			TabName = ets:new(Tab, [Type, Acc, named_table]),
			{ok, TabName}
	end.

%% Rename table
rename_tab(Tab, Name) ->
	ets:rename(Tab, Name).


%% update user collection (insert, delete)
update_usr_collection(User, Tab_info, insert) ->
	ets:insert(User, Tab_info);

update_usr_collection(User, Tab_info, delete) ->
	{Tab, _Options} = Tab_info,
	ets:delete(User, Tab).

	
%% 	
%% 	case Acctype of
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


