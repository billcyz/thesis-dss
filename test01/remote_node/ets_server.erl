%% @author billcyz
%% @doc @todo Add description to ets_server.


-module(ets_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/1]).
-export([create_table/1, insert_data/2, clear_table/1, stop_server/0]).

-record(state, {app}).

-define(SERVER, ?MODULE).

start(App) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [App], []).

init([App]) ->
	process_flag(trap_exit, true),
	create_ets(App),
	{ok, #state{app = App}}.

create_table(Tab) ->
	gen_server:call(?SERVER, {create_table, Tab}),
	ok.

insert_data(Tab, Data) ->
	gen_server:call(?SERVER, {insert_data, Tab, Data}),
	ok.

clear_table(Tab) ->
	gen_server:call(?SERVER, {clear, Tab}),
	ok.

stop_server() ->
	gen_server:stop(?SERVER, shutdown, 2),
	ok.

handle_call(Request, _From, State) ->
	case Request of
		{create_table, Tab} ->
			ets:new(Tab, [named_table]);
		{insert_data, Tab, Data} ->
			ets:insert(Tab, Data);
		{clear, Tab} ->
			ets:delete(Tab)
	end,
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Request, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

create_ets(App) ->
	ets:new(App, [named_table]),
	case ets:info(App) of
		undefined -> io:format("Can't create App table~n");
		_ -> ok
	end.


