%% @author billcyz
%% @doc @todo Add description to knk_comp_traffic.


-module(knk_comp_traffic).

-export([]).


%% ----------------------------------------------------------------------------

%% Start traffic process
-spec start_traffic() -> tuple() | 'ok'.
start_traffic() ->
	process_flag(trap_exit, true),
	Pid = spawn_link(?MODULE, start_init, []),
	true = register(knk_traffic, Pid),
	case whereis(knk_traffic) of
		undefined -> {error, traffic_process_not_started};
		_ -> ok
	end.

start_init() ->
	traffic_hub().

%% Traffic hub for managing traffic
traffic_hub() ->
	receive
		{msg, From, msg_got} ->
			1;
		{info, }

%% Find function in component. Function should have the name, and
%% numbers of arguments. [{fun_01, 3}]
-spec find_function(atom(), atom(), list()) -> list().
find_function(AppName, CompName, FunName) ->
	1.

%% Handshake between nodes
%% node_verify
node_verify(NodeL) ->
	
	receive
		{msg, From, msg_got} ->
			update_node_state(From),
			collect_info();
		{msg, From, }




