%% @author billcyz
%% @doc @todo Add description to knk_app.


-module(knk_app).
-behaviour(application).
-export([start/2, stop/1]).

%% Export for testing
-export([start_app_init/0, start_log/0,
		 prepare_dir/1]).

%% --------------------------------------------------------------
%% System Interface API
%% --------------------------------------------------------------

%% Start knk application
start(normal, _Args) ->
	start_app_init(),
	case knk_sup:start_link() of
		{ok, SupPid} ->
			AppName = knk_general:get_config(app),
			{ok, CompNode} = create_storage_node(node(), AppName),
			start_knk_server(CompNode, AppName),
			open_port(AppName),
%% 			case create_storage_node() of
%% 				ok -> 
%% 					%% Load or create ets table on storage node
%% 				no -> no
%% 			end,
			{ok, SupPid};
		E -> E
	end.

%% Requesting component distribution. Called from system interface.
%% Sending broadcast message among the network.
request_component() ->
	AppName = knk_general:get_config(app),
	Nic = knk_general:get_config(nic),
	[{_, LAddr}] = local_ip(Nic),
	
	%% Make knk server send distribute request to traffic handler
	case knk_general:check_process(traffic_handler) of
		ok -> traffic_handler:request_distribute(node(), LAddr, AppName);
		no -> throw("Traffic Handler is not found")
	end.

%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------

%% Stop knk application
stop(_State) -> ok.

%% Start KNK server
start_knk_server(CompNode, AppName) ->
	knk_sup:start_child_sup(knk_srv_sup, knk_srv_sup, [AppName, 
													   CompNode]).

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

%% Initialize before start knk application
start_app_init() ->
	case start_log() of
		{ok, _Pid} ->
			directory_check(prepare_dir([log, data, app_dir]));
		_ ->
			{error, initialize_failed}
	end.

%% Start logging service
start_log() ->
	knk_log:start(),
	case whereis(knk_log) of
		Pid when is_pid(Pid) ->
			{ok, Pid};
		undefined ->
			{error, log_failed}
	end.

%% Start knk port listener
open_port(AppName) ->
	AppPort = knk_general:get_config(app_port),
	KNKPort = knk_general:get_config(knk_port),
	knk_sup:start_child_sup(knk_socket_sup, knk_socket_sup, 
							[AppPort, KNKPort]).

%% Create knk component storage
create_storage_node(KNKName, AppName) ->
	[SysName, Host] = string:tokens(atom_to_list(node()), "@"),
	StorageNode = list_to_atom(SysName ++ "_" ++ "storage" ++ "@"
							  ++ Host),
	os:cmd(
	  lists:flatten(
		io_lib:format("erl -name ~p -s ~p ~p ~p ~p -hidden -detached", [StorageNode,
																		knk_etssup,
																		start_link,
																		KNKName, AppName]))),
	{ok, StorageNode}.
	

%% Get broadcast ip address according to network
%% interface
-spec broadcast_ip(any()) -> term().
broadcast_ip(Nic) ->
	{ok, Addrs} = inet:getifaddrs(),
	[{Nic, Addr} || {NicName, Opts} <- Addrs, NicName =:= Nic,
					{broadaddr, Addr} <- Opts, size(Addr) =:= 4].

%% Get ip address according to network interface
local_ip(Nic) ->
	{ok, Addrs} = inet:getifaddrs(),
	[{Nic, Addr} || {NicName, Opts} <- Addrs, NicName =:= Nic,
					{addr, Addr} <- Opts, size(Addr) =:= 4].




