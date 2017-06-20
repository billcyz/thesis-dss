%% @author billcyz
%% @doc @todo Add description to knk_general.

%% KNK general module provides basic function for the
%% KNK framework.

-module(knk_general).

-export([check_ets/1,
		 create_ets/2,
		 collect_node_info/0, node_candidate/1,
		 knk_local_broadcast/2, knk_node_abcast/3, knk_node_sbcast/3,
		 reply_msg/3]).

-export([basic_info/0, get_config/1]).

%% --------------------------------------------------------------

%% Get basic information of KNK system. The information includes
%% application name, node name, local ip address, and system status.
basic_info() ->
	{_, SysStatus} = knk_srv:check_sys_status(),
	{_, AppName} = knk_srv:check_app_name(knk_srv),
	{_, NodeIP} = knk_socket_srv:get_local_ip(),
	{basic_info, [node(), NodeIP, AppName, SysStatus]}.

%% Get knk application configuration
-spec get_config(atom()) -> list().
get_config(Env) ->
	{ok, Atts} = application:get_env(knk, Env),
	Atts.

%% Check process live or not
check_process(local, ProRef) ->
	case whereis(ProRef) of
		_Pid -> ok;
		undefined -> no
	end;

check_process(global, ProRef) ->
	case global:whereis_name(ProRef) of
		_Pid -> ok;
		undefined -> no
	end.

%% Create KNK group 
create_group(AppName) ->
	AppName.

%% Join KNK group
-spec join_group(tuple()) -> 'ok'.
join_group(ReceiverIP) ->
	ReceiverIP.

%% Check ets table
-spec check_ets(atom()) -> 'ok' | 'tab_exist'.
check_ets(Tab) ->
	case ets:info(Tab) of
		undefined -> {tab_notexist};
		[_] -> {tab_exist}
	end.

%% Create ets table
-spec create_ets(atom(), list()) -> 'ok' | {info, ets_exist}.
create_ets(Tab, [Type, Access, NameType, Options]) ->
	case check_ets(Tab) of
		{tab_notexist} ->
			ets:new(Tab, [Type, Access, NameType,
						  Options]),
			ok;
		_ -> {info, ets_exist}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start node

%% Get available nodes
%% Broadcast? Send request to father node, then get connected nodes
%% from father node.
-spec get_node(atom()) -> list().
get_nodes(FNode) ->
	[FNode].

%% Random character combination
-spec get_id(atom()) -> list().
%%get_id(AppName) ->
rand_char_combin() ->
	NC_tuple = {0,1,2,3,4,5,6,7,8,9,
			   a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,
			   u,v,w,x,y,z},
	Size = size(NC_tuple),
	rand:seed(exsplus, {erlang:unique_integer([positive, monotonic]),
						epoch_time(), rand:uniform(10)}),
	F = fun(_, R) ->
				[element(rand:uniform(Size), NC_tuple) | R]
		end,
	IdList = lists:foldl(F, "", lists:seq(1, 6)),
	ComId = lists:concat(IdList),
	
	case ets:lookup(AppName, ComId) of
		[] -> ComId;
		_ -> get_id(AppName)
	end.

%% Epoch time
epoch_time() ->
	calendar:datetime_to_gregorian_seconds(
	  calendar:now_to_universal_time(os:timestamp())) -
		719528 * 24 * 3600.

%% Generate unique id
%% Random character combination and epoch timestamp, split into three parts, 
%% then transfer position and combine into 12 bits combination. 
gen_uniid() ->
	%%<<I1:32, I2:32, I3:32>> = crypto:strong_rand_bytes(12),
	rand:seed(exsplus, {erlang:unique_integer([positive, monotonic]),
						epoch_time(), rand:uniform(10)}),
	1.
	
%% Broadcast message
%% Broadcast message to Pid in local asynchronously
-spec knk_local_broadcast(tuple(), [pid() | atom()]) -> 'ok'.
knk_local_broadcast(Msg, ServRef) ->
	lists:foreach(fun(Pid) -> 
						  Pid ! Msg end, ServRef),
	ok.

%% Broadcast message to Pid in remote node asychronously
-spec knk_node_abcast([node()], pid() | atom(), tuple()) -> 'ok'.
knk_node_abcast(NodeL, ServRef, Msg) ->
	rpc:abcast(NodeL, ServRef, Msg),
	ok.

%% Broadcast message to Pid in remote node sychronously
-spec knk_node_sbcast([node()], pid() | atom(), tuple()) -> 'ok'.
knk_node_sbcast(NodeL, ServRef, Msg) ->
	rpc:sbcast(NodeL, ServRef, Msg),
	ok.

%% Node candidate
%% [{number, node()}]
-spec node_candidate(list()) -> list() | tuple().
node_candidate(NodeL) ->
	lists:seq(1, erlang:length(NodeL)),
	rand:seed(exsplus, {erlang:unique_integer([positive, monotonic]),
						epoch_time(), rand:uniform(10)}),
	case erlang:length(is_list(NodeL)) of
		0 -> {error, no_node_candidate};
		1 -> NodeL;
		_ ->
			lists:last(lists:keysort(1, sign_random(NodeL, [])))
	end.

%% Sign random number to each node
sign_random([NodeH | NodeT], RList) ->
	sign_random(NodeT, RList ++ [{rand:uniform(1000), NodeH}]);

sign_random([], RList) -> RList.

%% Collect node information
%% Get node name, cookie, and components inside the node
-spec collect_node_info() -> list().
collect_node_info() ->
	[node(), erlang:get_cookie(), 
	 knk_comp_lookup:lookup_localappcomp(knk_comp_lookup:lookup_localapp())].

%% Reply message to process with AppName, NodeName, Component Name, and Function
%% Name. It is used to send and update information to server process.
-spec reply_msg(atom() | pid(), tuple(), {any()}) -> any().
reply_msg(PidRef, {[From, {A, N, C, F}]}, {Msg}) ->
	try
		PidRef ! {[From, {A, N, C, F}], Msg},
		receive
			Info -> Info
		end
	catch
		error:Error -> {error, Error}
	end.
	





