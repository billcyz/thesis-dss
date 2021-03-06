%% @author billcyz
%% @doc @todo Add description to com_db_srv.


%% Script name: com_db_gs.erl

%% Simple bank system

%% Register node on the server and create schema by using Mnesia. The
%% database shoule be able to run simple SQL query, such as read, write,
%% and update. Instead, multiple databases can be created on one single
%% machine.

-module(com_db_gs).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([register_node/1]).

-record(user, {name, location}).
-record(balance, {name, balance=0}).

%% Start database process
start() ->
	register(dbcom, spawn(?MODULE, init, [])).

%% Create new account
new_account(Who) ->
	gen_server:call(?MODULE, {new, Who}).

%% Remove account
remove_account(Who) ->
	gen_server:call(?MODULE, {remove, Who}).

%% Deposit money
deposit(Who, Amount) ->
	gen_server:call(?MODULE, {add, Who, Amount}).

%% Withdraw money
withdraw(Who, Amount) ->
	gen_server:call(?MODULE, {remove, Who, Amount}).

%% Create new database
new_db(Db) ->
	gen_server:call(?MODULE, {new, Db}).

%% Initilaze table
init_tab(Tab) ->
	gen_server:call(?MODULE, {init, Tab}).

%% Process sql query
%%process_query(Query)

%% %% Start node for Mnesia
%% register_node(Db) ->
%% 	case net_kernel:start([Db, shortnames]) of
%% 		{ok, _Pid} -> ok;
%% 		{error, Reason} -> Reason
%% 	end.
%% 
%% %% Create database
%% create_db(Db) ->
%% 	case ?MODULE:register_node(Db) of
%% 		ok ->
%% 			case mnesia:create_schema([node()]) of
%% 				ok -> {ok, success};
%% 				{error, Reason} -> Reason
%% 			end;
%% 		_Other -> {error}
%% 	end.
%% 
%% create_tab() ->
%% 	mnesia:create_table(user, [{attributes, record_info(fields, user)}]),
%% 	mnesia:create_table(balance, [{attributes, record_info(fields, balance)}]),
%% 	mnesia:stop().
%% 
%% start_db() ->
%% 	mnesia:start(),
%% 	mnesia:wait_for_tables([user, balance], 20000).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
	%% Listen to port 13306
	case gen_tcp:listen(13306, ?TCP_OPTIONS) of
		{ok, LSocket} ->
			{ok, accept(LSocket)};
		{error, Reason} ->
			{stop, Reason}
	end.
	

%% example of handle_call
handle_call({new, Who}, _From, Tab) ->
	Reply = case ets:lookup(Tab, Who) of
				[] -> ets:insert(Tab, {Who, 0}),
					  {welcome, who};
				[_] -> {Who, you_already_are_a_customer}
			end,
	{reply, Reply, Tab};

handle_call({new, Who}, _From, State) ->
	Reply = case com_db_srv:check_account(Who) of
				{account_exists} ->
					{account_exists};
				{account_not_exists} ->
					com_db_srv:new_account(Who)
			end,
	{reply, Reply, State};

handle_call({remove, Who}, _From, State) ->
	Reply = case com_db_srv:check_account(Who) of
				{account_exists} ->
					com_db_srv:dele_account(Who);
				{account_not_exists} ->
					{account_not_exists}
			end,
	{reply, Reply, State};

handle_call({add, Who, Amount}, _From, State) ->
	Reply = case com_db_srv:check_account(Who) of
				{account_exists} ->
					com_db_srv:deposit(Who, Amount),
					com_db_srv:check_balance(Who);
				{account_not_exists} ->
					{account_not_exists}
			end,
	{reply, Reply, State};

handle_call({remove, Who, Amount}, _From, State) ->
	Reply = case com_db_srv:check_account(Who) of
				{account_exists} ->
					com_db_srv:withdraw(Who, Amount)
					com_db_srv:check_balance(Who);
				{account_not_exists} ->
					{account_not_exists}
			end,
	{reply, Reply, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

init() ->
	db_loop().

db_loop() ->
	receive
		{query, create_db, DB} ->
			%% Create database
			create_db(DB),
			db_loop()
	end.
