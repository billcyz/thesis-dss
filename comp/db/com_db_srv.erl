%% @author billcyz
%% @doc @todo Add description to com_db_srv.


%% Script name: com_db_srv.erl
%% Register node on the server and create schema by using Mnesia. The
%% database shoule be able to run simple SQL query, such as read, write,
%% and update. Instead, multiple databases can be created on one single
%% machine.

-module(com_db_srv).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([register_node/1]).

start() ->
	register(dbcom, spawn(?MODULE, init, [])).

%% Process sql query
process_query(Query)

register_node(Db) ->
	case net_kernel:start([Db, shortnames]) of
		{ok, _Pid} -> ok;
		{error, Reason} -> Reason
	end.

create_db(Db) ->
	case ?MODULE:register_node(Db) of
		ok ->
			case mnesia:create_schema([node()]) of
				ok -> {ok, success};
				{error, Reason} -> Reason
			end;
		_Other -> {error}
	end.


%% ====================================================================
%% Behavioural functions
%% ====================================================================



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
