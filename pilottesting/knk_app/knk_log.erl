%% @author billcyz
%% @doc @todo Add description to knk_log.

%% gen_event:add_handler(?MODULE, ?MODULE, []).
%% test_event_call:write_log(normal, lists:flatten(io_lib:format("hello, ~p~n", [self()]))).

-module(knk_log).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_info/2, handle_call/2,
		 terminate/2, code_change/3]).

-export([start/0, write_log/2]).

-define(SERVER, ?MODULE).
%%-define(LOGFILE, "C:/Users/billcyz/Desktop/abc.log").

%% --------------------------------------------------------------

%% Start knk event manager
start() ->
	{ok, Pid} = gen_event:start_link({local, ?SERVER}),
	gen_event:add_handler(?SERVER, ?MODULE, []),
	{ok, Pid}.

handle_event({normal_log, Msg, IoD}, State) ->
	io:format(IoD, "[~p] ~p~n", [format_time(), Msg]),
	close_log_file(IoD),
	{ok, State};

handle_event({error_log, Msg, IoD}, State) ->
	io:format(IoD, "[~p] [error] ~p~n", [format_time(), Msg]),
	close_log_file(IoD),
	{ok, State};

handle_event({warning_log, Msg, IoD}, State) ->
	io:format(IoD, "[~p] [warning] ~p~n", [format_time(), Msg]),
	close_log_file(IoD),
	{ok, State}.

handle_info(_, State) ->
	{ok, State}.

handle_call(_, State) ->
	{ok, State}.

terminate(_, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

init([]) ->
	{ok, []}.

%% Edit log file
-spec open_log_file() -> tuple().
open_log_file() ->
	LogDir = application:get_env(knk, log),
	case file:open(lists:concat([LogDir, "/", "knk.log"]), [append]) of
		{ok, IoD} -> {ok, IoD};
		E -> E
	end.

%% CLose edit operation
-spec close_log_file(any()) -> any().
close_log_file(IoDevice) ->
	file:close(IoDevice).

%% Geerate timestamp for logging
-spec format_time() -> list().
format_time() ->
	{{Y, M, D}, {Hr, Min, Sec}} = calendar:local_time(),
	Ymd = string:join([integer_to_list(Y), integer_to_list(M),
					   integer_to_list(D)], "-"),
	Hms = string:join([integer_to_list(Hr), integer_to_list(Min),
					   integer_to_list(Sec)], ":"),
	string:join([Ymd, Hms], " ").

%% Write log
write_log(LogType, Msg) ->
	case LogType of
		normal ->
			{ok, IoD} = open_log_file(),
			log_fun({normal_log, Msg, IoD}, 1);
		error ->
			{ok, IoD} = open_log_file(),
			log_fun({error_log, Msg, IoD}, 2);
		warning ->
			{ok, IoD} = open_log_file(),
			log_fun({warning_log, Msg, IoD}, 3)
	end.

log_fun({normal_log, Msg, IoD}, _State) ->
	gen_event:notify(?SERVER, {normal_log, Msg, IoD});

log_fun({error_log, Msg, IoD}, _State) ->
	gen_event:notify(?MODULE, {error_log, Msg, IoD}).




