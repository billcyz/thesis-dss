%% @author billcyz
%% @doc @todo Add description to com_db_srv.

%% Script name: com_db_srv.erl
%% Main function of database component

-module(com_db_srv).
-include_lib("stdlib/include/qlc.hrl").

-record(user, {name}).
-record(balance, {name, balance=0}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_db/0, new_account/1, dele_account/1, check_account/1, deposit/2, withdraw/2, check_balance/1]).

%% Initialize database
create_db() ->
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
new_account(Name) ->
	NA = fun() -> 
				 mnesia:write(#user{name=Name}) 
		 end,
	mnesia:transaction(NA).

%% Delete account
dele_account(Name) ->
	DA = fun() ->
				 mnesia:delete({user, Name})
		 end,
	case mnesia:transaction(DA) of
		ok -> {account_deleted};
		_ -> {no_such_account}
	end.

%% Check account existence
check_account(Name) ->
	CA = fun() ->
				 mnesia:read({user, Name})
		 end,
	case mnesia:transaction(CA) of
		[_] -> {account_exists};
		_Other -> {account_not_exists}
	end.

%% Deposit money
deposit(Name, Amount) ->
	case ?MODULE:check_account(Name) of
		{account_exists} ->
			[_Name, Balance] = ?MODULE:check_balance(Name),
			NewBalance = Balance + Amount,
			DP = fun() ->
						 mnesia:write(#balance{name=Name,
											   balance=NewBalance})
				 end,
			mnesia:transaction(DP);
		{account_not_exists} -> {account_not_exists}
	end.
	
%% Withdraw money
withdraw(Name, Amount) ->
	[_Name, Balance] = ?MODULE:check_balance(Name),
	if
		Balance < Amount  ->
			{not_enough_money};
		true ->
			NewBalance = Balance - Amount,
			WT = fun() ->
						 mnesia:write(#balance{name=Name,
											   balance=NewBalance})
				 end,
			mnesia:transaction(WT)
	end.

%% Check balance
check_balance(Name) ->
	case check_account(Name) of
		{account_exists} -> 
			CK = fun() ->
						 mnesia:read({balance, Name})
				 end,
			[Name, _Balance] = mnesia:transaction(CK);
		{account_not_exists} ->
			{account_not_exists}
	end.



%% ====================================================================
%% Internal functions
%% ====================================================================


