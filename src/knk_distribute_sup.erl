%% @author billcyz
%% @doc @todo Add description to knk_distribute_sup.

%% Supervisor for distribution service. Once knk system
%% receives request to transfer files or test component availability,
%% then this supervisor starts another distribute worker to handle
%% the next incoming request

-module(knk_distribute_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/2, start_temp_distsrv/0, assign_dist_task/1]).

-define(SERVER, ?MODULE).

%% ---------------------------------------------------------------------------------

%% Start distribution supervisor
-spec start_link(atom(), atom()) -> tuple().
start_link(KNKName, AppName) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [KNKName, AppName]).

init([KNKName, AppName]) ->
	spawn_link(fun start_dist_pool/0),
	{ok, {{simple_one_for_one, 0, 1},
		  [{knk_distribute,
			{knk_distribute, start_link, [KNKName, AppName]},
			temporary, 1000, worker, [knk_distribute]}]}}.

%% Start distribution pool process
-spec start_dist_pool() -> list().
start_dist_pool() ->
	[start_temp_distsrv() || _ <- lists:seq(1, 3)].

%% Start distribution process
start_temp_distsrv() ->
	supervisor:start_child(?MODULE, []).

%% Get all temporary processes for distribution
-spec all_children() -> list() | atom().
all_children() ->
	case whereis(?SERVER) of
		undefined -> sup_not_started;
		_Pid -> supervisor:which_children(?SERVER)
	end.

%% Assign tasks to distribute process
assign_dist_task(Data) ->
	case select_finished_process(all_children()) of
		no_pid_to_kill -> ok;
		{ok, KPid} -> stopped = gen_server:call(KPid, stop)
	end,
	{ok, Pid} = select_available_process(all_children()),
	start_temp_distsrv(),
	io:format("Assign task to ~p~n", [Pid]),
	gen_server:call(Pid, Data).

%% Select available process
select_available_process([ChildH | ChildT]) ->
	{undefined, Pid, worker, [distribute_worker]} = ChildH,
	case gen_server:call(Pid, check_status) of
		active ->
			io:format("Find available children ~p~n", [Pid]),
			{ok, Pid};
		running -> select_available_process(ChildT);
		done -> select_available_process(ChildT)
	end.

%% Select temporary process to stop
select_finished_process([ChildH | ChildT]) ->
	{undefined, KPid, worker, [distribute_worker]} = ChildH,
	case gen_server:call(KPid, check_status) of
		done -> 
			io:format("Find process ~p to kill~n", [KPid]),
			{ok, KPid};
		_ -> select_finished_process(ChildT)
	end;
select_finished_process([]) -> no_pid_to_kill.

