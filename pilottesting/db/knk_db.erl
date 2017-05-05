%% @author billcyz
%% @doc @todo Add description to knk_db.

%% KNK system database. mnesia

-module(knk_db).

-export([]).

-include("db.hrl").

%% Check Mnesia running
ensure_mnesia_running() ->
	case mnesia:system_info(is_running) of
		yes -> ok;
		starting ->
			wait_for(ensure_mnesia_running),
			ensure_mnesia_running();
		Reason when Reason =:= no; Reason =:= stopping ->
			throw({error, mnesia_not_running})
	end.

%% Create schema.
create_schema() ->
	mnesia:create_schema([node()]).

start() ->
	mnesia:start().

%% Populate table
init() ->
	mnesia:create_table(server, [{attributes, record_info(fields, server)}]),
	mnesia:create_table(component, [{attributes, record_info(fields, component)}]).


wait_for(Condition) ->
	knk_log:info("Waiting for ~p...~n", [Condition]),
	timer:sleep(1000).

