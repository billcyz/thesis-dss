%% @author billcyz
%% @doc @todo Add description to knk_select_monitor.

%% Porcess monitors select_pool table, which stores received
%% tar file information. The monitor will keep monitoring the
%% select_pool table until the table is not empty. 
%%
%% If the table is not empty, then the process will monitor
%% for two more times to get more data.

-module(knk_select_monitor).

-export([start_monitor/0, monitor_loop/1, table_check_loop/0]).

%% ------------------------------------------------------------------

%% Start monitor process and table process
start_monitor() ->
	case ets:info(selection_pool) of
		undefined -> pool_undefined;
		[_] ->
			Pid = spawn_link(?MODULE, monitor_loop, [0]),
			register(monitor_loop, Pid),
			TabChPid = spawn_link(?MODULE, table_check_loop, []),
			register(table_check_loop, TabChPid),
			activate_monitor()
	end.

%% Activate monitor process
activate_monitor() ->
	monitor_loop ! start.

%% Send signal to table process after 3 seconds
send_check_signal() ->
	erlang:send_after(3000, table_check_loop, check_table).

%% Send monitor result
send_notification() ->
	{_, KNKNode} = knk_srv:check_knk(knk_etssrv),
	gen_server:cast({knk_distribute, KNKNode}, {distribute, file_detected}).

%% Monitor process
monitor_loop(N) ->
	receive
		start ->
			table_check_loop ! check_table,
			monitor_loop(N);
		'$end_of_table' ->
			send_check_signal(),
			monitor_loop(N);
		_Other ->
			if
				N < 3 ->
					send_check_signal(),
					monitor_loop(N + 1);
				true ->
					%% tells distribute process that files are coming
					send_notification(),
					table_check_loop ! stop,
					exit(normal)
			end
	end.

%% Table check process
table_check_loop() ->
	receive
		check_table ->
			Result = gen_server:call(knk_etssrv, check_select),
			monitor_loop ! Result,
			table_check_loop();
		stop ->
			exit(normal)
	end.
