%% @author billcyz
%% @doc @todo Add description to knk_distribute.

%% Distribution part for KNK system.

-module(knk_distribute).

-export([distribute_init/0,
		 create_count_tab/0, update_count/1,
		 comp_candidate/2]).

%% ----------------------------------------------------------------------

%% Initialize distribution part
distribute_init() ->
	ok = create_count_tab(),
	case string:str(
		   os:cmd("nc -z -v 127.0.0.1 22"), "succeeded") of
		0 -> {error, ssh_port_unavailable};
		_ ->
			check_extra_service([rsync, ssh, md5sum, tar])
	end.

%% Check required service on sevrer, such as SSH and md5sum
check_extra_service(ServiceL) ->
	ExtServL = [os:cmd(lists:flatten(io_lib:format("which ~p", [X]))) 
		|| X <- ServiceL],
	check_empty_service(ServiceL, ExtServL, []).

check_empty_service([ServH | ServT], [H | T], E) ->
	if
		H =:= [] -> check_empty_service(ServT, T, 
										E ++ [err_result(ServH)]);
		H =/= [] -> check_empty_service(ServT, T, E)
	end;
check_empty_service([], [], E) ->
	if
		length(E) =/= 0 -> E;
		true -> ok
	end.

err_result(ServH) ->
	{error, lists:concat([ServH, "_", unavailable])}.

%% Create distribute count table
%% [dist_count, AppName, {CompName, Count}]
-spec create_count_tab() -> 'ok' | tuple().
create_count_tab() ->
	ets:new(dist_count, [bag, private, named_table]),
	case ets:info(dist_count) of
		undefined -> {error, count_tab_failed};
		_ -> ok
	end.

%% Update count table
%%%% Should consider receiving unknown components %%%%
-spec update_count(atom(), list()) -> tuple().
update_count(AppName, CompL) ->
	lists:map(
	  fun(CompName) -> 
			  [{_, Count}] = ets:lookup(dist_count, CompName), 
			  ets:insert(dist_count, {CompName, Count + 1}) end, 
	  CompL),
	ok.

%% Distribute component to destination
-spec distribute(atom(), atom(), [node()]) -> 'ok'.
distribute(AppName, CompName, Dst) ->
	1.

%% Generate hash value for components that going to be
%% distributed
gen_hash() ->
	1.

%% Select components to be distributed. Components with the least numbers 
%% will be distributed. If the components to be distribuetd are not in the 
%% storage, then ignore the message and send no reply.
%% [{comp_01, 2}, {comp_02, 3}]  
-spec comp_candidate(atom(), integer()) -> list().
comp_candidate(AppName, Range) ->
	lists:sublist(lists:keysort(2, ets:lookup_element(dist_count, AppName, 2)),
				  Range).

%% 




