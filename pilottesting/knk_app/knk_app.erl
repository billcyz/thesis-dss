%% @author billcyz
%% @doc @todo Add description to knk_app.


-module(knk_app).
-behaviour(application).
-export([start/2, stop/1]).

-export([]).

%% --------------------------------------------------------------

%% Initialize before start knk application
start_app_init() ->
	case start_log() of
		{ok, _} ->
			directory_check(prepare_dir([log, data, user_app])),
			epmd_check();
		_ ->
			{error, initialize_failed}
	end.

%% Start knk application
start(normal, _Args) ->
	start_app_init(),
	case knk_sup:start_main_sup() of
		{ok, SupPid} ->
			open_port(),
			prepare_table(),
			{ok, SupPid};
		E -> E
	end.

%% 	case whereis(knk_sup) of
%% 		Pid when Pid =:= SupPid ->
%% 			knk_log:write_log(normal,
%% 							  lists:flatten(io_lib:format("KNK application main supervisor ~p started~n", 
%% 														  [Pid])));
%% 		undefined ->
%% 			knk_log:write_log(error,
%% 							  lists:flatten(io_lib:format("KNK application main supervisor error~n", 
%% 														  [])))
%% 	end,
%% 	{ok, SupPid}.

%% Stop knk application
stop(_State) -> ok.

%% Check epmd service status
%% count process number: ps aux | grep epmd | wc -l
%% get pid: pgrep epmd
-spec epmd_check() -> 'ok'.
epmd_check() ->
	case os:cmd("pgrep epmd") of
		[] ->
			knk_log:write_log(warning, 
							  lists:flatten(io_lib:format("Epmd service is not running...~n", 
														  []))),
			knk_log:write_log(normal, 
							  lists:flatten(io_lib:format("Starting epmd service...~n", 
														  []))),
			os:cmd("epmd &");
		_ -> 
			knk_log:write_log(normal,
							  lists:flatten(io_lib:format("Epmd service is running...~n",
														  [])))
	end,
	ok.

%% Check compulsory directory
%% /log, /user_app
-spec directory_check(list()) -> 'ok'.
directory_check([DirH | DirT]) ->
	{AttN, Atts} = DirH,
	case filelib:is_dir(Atts) of
		true ->
			knk_log:write_log(normal,
							  lists:flatten(io_lib:format("~p directory dected~n",
														  [AttN]))),
			directory_check(DirT);
		false -> 
			knk_log:write_log(error,
							  lists:flatten(io_lib:format("~p directory not found~n",
														  [AttN]))),
			S = io_lib:format("mkdir ~p", [Atts]),
			os:cmd(lists:flatten(S))
	end;

directory_check([]) -> ok.

%% Prepare log and user_app directory
-spec prepare_dir(list()) -> list().
prepare_dir(DirL) ->
	[{DirName, Atts} ||
	 DirName <- DirL, {ok, Atts} <- application:get_env(knk, DirName)].

%% Start logging service
start_log() ->
	{ok, LogPid} = knk_log:start(),
	case whereis(knk_log) of
		{ok, _Pid} ->
			{ok, LogPid};
		undefined ->
			{error, log_failed}
	end.

%% Start knk port listener
open_port() ->
	1.

%% Prepare knk ets table
prepare_table() ->
	2.







