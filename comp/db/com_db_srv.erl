%% @author billcyz
%% @doc @todo Add description to com_db_srv.

%% Script name: com_db_srv.erl
%% Main function of database component

-module(com_db_srv).
-include_lib("stdlib/include/qlc.hrl").

-record(user, {name, location}).
-record(balance, {name, balance=0}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

%% Initialize database
create_db(Db) ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(type, user)
	catch
		exit: _ ->
			mnesia:create_table(user, [{attributes, record_info(fields, user)}])
	end,
	try
		mnesia:table_info(type, balance)
	catch
		exit: _ ->
			mnesia:create_table(balance, [{attributes, record_info(fields, balance)},
								{type, bag},
								{disc_copies, [node()]}])
	end,
	mnesia:wait_for_tables([user, balance], 20000).

%% Create new account
new_account(Name, Location) ->
	NA = fun() -> 
				 mnesia:write(#user{name=Name, location=Location}) 
		 end,
	mnesia:transaction(NA).

%% Delete account
dele_account(Name, Location) ->
	DA = fun() ->
				 mnesia:delete({usr, Name, Location})
		 end,
	case mnesia:transaction(DA) of
		ok -> {account_deleted};
		_ -> {no_such_account}
	end.

%% Check account existence
check_account(Name, Location) ->
	CA = fun() ->
				 mnesia:read({user, Name, Location})
		 end,
	case mnesia:transaction(CA) of
		[_] -> {account_exists};
		_Other -> {account_not_exists}
	end.

%% Deposit money
deposit(Name, Amount) ->
	1.

%% Withdraw money

%% Check balance

%% ====================================================================
%% Internal functions
%% ====================================================================


