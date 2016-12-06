%% @author billcyz
%% @doc @todo Add description to cache_fun.

%% Script name: comp_cache_service.erl
%% Server script to perform all functions.

-module(comp_cache_service).
-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
%%-export([]).
-compile(export_all).

start() ->
	%%{ok, LSocket} = gen_tcp:listen(7973, [{active, false},{packet,2}]),
	
	case gen_tcp:listen(7973, [{active, false},{packet,2}]) of
		{ok, ListenSocket} ->
			LSocket = ListenSocket,
			%% Start receiving loop
			LSocket;
		{error, Reason} ->
			%% Return Reason
			Reason
	end.


%% Create ets table
%% Apply user permission to each ets table (private, local, share)
%% Each user has their own ets table collection
%% user collection should be "bag" type

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% create user collection, which includes all tables related to the user
%% user collection & permissions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_usr_collection(User) ->
	Tid = ets:new(User, [named_table, public, bag]),
	{ok, Tid}.

%% check user collection exists
chk_usr_collection(User) ->
	case ets:info(User) of
		[_] ->
			{User, collect_exists};
		undefined ->
			{User, no_collection}
	end.

%% update user collection (insert, delete)
update_usr_collection(User, Tab_info, insert) ->
	ets:insert(User, Tab_info);

update_usr_collection(User, Tab_info, delete) ->
	{Tab, _Options} = Tab_info,
	ets:delete(User, Tab).

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

%% Get all values in table


%% ====================================================================
%% Internal functions
%% ====================================================================


