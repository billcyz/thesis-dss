%% @author billcyz
%% @doc @todo Add description to transfer.

%% 15> A.                                                                     
%% "d41d8cd98f00b204e9800998ecf8427e a\n"
%% 16> 
%% 16> string:tokens(A, " \n").
%% ["d41d8cd98f00b204e9800998ecf8427e","a"]

-module(transfer).
-behaviour(gen_server).
%-export([start_link/0, init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).
%-export([store_file/2, request_file/0]).
-compile(export_all).

-record(state, {sys_status, socket, local_ip}). %% new, active

-define(SERVER, ?MODULE).
-define(NIC, "ens33").
-define(PORT, 10000).
-define(RANGE, 3).
-define(USER, tester).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	{ok, Socket} = gen_udp:open(?PORT, [binary, 
										{active, true}, 
										{reuseaddr, true},
										{broadcast, true}]),
	[{_, LAddr}] = local_ip(),
	create_ets(),
	{ok, #state{sys_status = new, socket = Socket, local_ip = LAddr}}.

store_file(FileName, FileSrc) ->
	gen_server:call(?SERVER, {store, FileName, FileSrc}).

request_file() ->
	gen_server:cast(?SERVER, request_file).

handle_request_file({request_file, RemoteNode}) ->
	gen_server:call(?SERVER, {request_file, RemoteNode}).

handle_cast(request_file, #state{socket = Socket} = State) ->
	[{_, BAddr}] = broadcast_ip(),
	gen_udp:send(Socket, BAddr, ?PORT, lists:flatten(io_lib:format("[~p, request_file]", 
																   [node()]))),
	{noreply, State}.

handle_info({udp, _Socket, Addr, _Port, Data}, #state{local_ip = LAddr} = State) ->
	if
		Addr =:= LAddr ->
			ok;
		Addr =/= LAddr ->
			io:format("Get data ~p from ~p~n", [Data, Addr]),
			parse_data(Data)
	end,
	{noreply, State};
handle_info({transfer_file, TargetTar}, State) ->
	io:format("Received tar file ~p~n", [TargetTar]), %% check file hash value
	{noreply, State}.

handle_call({store, FileName, FileSrc}, _From, State) ->
	case check_file_existence(FileSrc) of
		"ok\n" -> 
			ets:insert(fileinfo, {FileName, FileSrc}),
			ets:insert(filecount, {FileName, 1}),
			{reply, file_stored, State};
		"no\n" -> 
			io:format("File ~p not found~n", [FileSrc]),
			{reply, file_not_found, State}
	end;
handle_call({request_file, RNode}, _From, State) ->
	io:format("Start handling request_file call request~n"),
	WantedList = sort_count_list(), %% get file list that should be transfered
	io:format("Wanted list is ~p~n", [WantedList]),
	FileList = "/home/tester/temp_process/file_list.csv",
	io:format("Start to prepare file ~p ~p~n", [WantedList, FileList]),
	case prepare_file(WantedList, FileList) of
		ok ->
			%% compress and transfer
			TarHash = os:cmd(lists:flatten(
							   io_lib:format(
								 "`which bash` /home/tester/temp_process/distribute_transfer.sh prepare ~p ~p", 
								 [node(), FileList]))),
			{ok, TN} = store_tarhash(TarHash),
			transfer_file(RNode, TN),
			
			%% send file transfered signal to desitnation
			rpc:call(RNode, transfer_beta, , Args)
			
			{reply, file_transfered, State};
		no_file_to_transfer ->
			{reply, no_file_to_transfer, State}
	end.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.


%% use rpc, and gen_server:call on receiver side
notify_file_arrive(RNode, TarFile) ->
	1.

create_ets() ->
	ets:new(fileinfo, [named_table]),
	ets:new(filecount, [named_table]),
	ets:new(tarhash, [named_table]).

%% Get full list of distribution count
%% [{CompName, CompCount}]
distribute_list() ->
	case ets:first(filecount) of
		'$end_of_table' -> empty_dist_count;
		HComp ->
			distribute_list(HComp, [])
	end.

distribute_list(HComp, RList) ->
	HCompCountL = ets:lookup(filecount, HComp),
	case ets:next(filecount, HComp) of
		'$end_of_table' -> RList ++ HCompCountL;
		NComp ->
			distribute_list(NComp, RList ++ HCompCountL)
	end.

%% Get sorted file list
sort_count_list() ->
	case distribute_list() of
		DistL when is_list(DistL) ->
			[X || {X, _} <- lists:sublist(lists:keysort(2, DistL), ?RANGE)];
		empty_dist_count -> no_count_list
	end.

%% Store tar file md5 hash value
store_tarhash(Hash) ->
	[Value, TarFile] = string:tokens(Hash, " \n"),
	ets:insert(tarhash, {TarFile, Value}),
	{ok, TarFile}.

%% Prepare file, create file list to compress
prepare_file([FH | FT], FileList) ->
	case ets:lookup(fileinfo, FH) of
		[] -> prepare_file(FT, FileList);
		[{FH, FHP}] ->
			file:write_file(FileList, lists:flatten(io_lib:format("~p,~p~n", [FH, list_to_atom(FHP)])), [append]),
			prepare_file(FT, FileList)
	end;
prepare_file([], FileList) ->
	case filelib:is_file(FileList) of
		true -> ok;
		false -> no_file_to_transfer
	end.

transfer_file(RNode, TargetTar) ->
	[_, Host] = string:tokens(atom_to_list(RNode), "@"),
	DstHost = list_to_atom(Host),
	os:cmd(lists:flatten(
			 io_lib:format("`which bash` /home/tester/temp_process/distribute_transfer.sh transfer ~p ~p ~p ~p", 
						   [node(), ?USER, DstHost, TargetTar]))).
		
	%{transfer_beta, RNode} ! {transfer_file, TargetTar}. %% send notification to requestor that file has transfered

check_file_existence(FileSrc) ->
	os:cmd(lists:flatten(
			 io_lib:format("`which bash` /home/tester/temp_process/distribute_transfer.sh check_existence ~p", 
									   [FileSrc]))).

parse_data(Data) ->
	DataL = binary_to_list(Data),
	{ok, Token, _} = erl_scan:string(DataL ++ "."),
	{ok, Term} = erl_parse:parse_term(Token),
	direct_traffic(Term).

direct_traffic(InCome) ->
	case InCome of
		[RemoteNode, request_file] ->
			io:format("Received request_file Request from ~p~n", [RemoteNode]),
			handle_call({request_file, RemoteNode}, undefined, #state{sys_status = new});
		[RemoteNode, file_transfered, FileInfo] ->
			io:format("Received file from Node ~p~n", [RemoteNode]),
			handle_call({file_transfered, RemoteNode, FileInfo}, undefined, #state{sys_status = new})
	end.

local_ip() ->
	{ok, Addrs} = inet:getifaddrs(),
	[{?NIC, Addr} || {NicName, Opts} <- Addrs, NicName =:= ?NIC,
					{addr, Addr} <- Opts, size(Addr) =:= 4].

broadcast_ip() ->
	{ok, Addrs} = inet:getifaddrs(),
	[{?NIC, Addr} || {NicName, Opts} <- Addrs, NicName =:= ?NIC,
					{broadaddr, Addr} <- Opts, size(Addr) =:= 4].

prepare_ip(IP) ->
	%%[{_, IP}] = local_ip(),
	[IPA, IPB, IPC, IPD] = string:tokens(lists:flatten(io_lib:format("~p", [IP])), "{,}"),
	list_to_atom(IPA ++ "." ++ IPB ++ "." ++ IPC ++ "." ++ IPD).
