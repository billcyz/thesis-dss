%% @author billcyz
%% @doc @todo Add description to knk_comp_def.

%% Defines components of the application. Each component is the user-defined
%% function. Developers should provide the information of functions.
%%
%% Each function requires one unique function id, (and one virtual path -> symbolic
%% link) for locating the function, function hash code (authentication), function
%% type (Read(R), Write(W), Delete(D), Update(U)).
%%
%% When one component (function) wants to communicate with other components, the
%% requestor component should provide the target component's type and id, then the
%% traffic handler will search through the function storage according to types and 
%% unique component id. The search result will be used for forwarding traffic. 





-module(knk_comp_def).


-export([check_comp_ets/1]).


%% Store component names into ets table
%% sys table is the system table, it starts when the application is start up.
%% The sys ets table requires the service name, service component name, and pid
%% of each component. Update components info in compdb.
make_comp_ets([sys, Service, ServiceComp, Pid, Host]) ->
	ets:insert(sys, {Service, ServiceComp, Pid, Host}),
	ok.
	
%% Check Pid and Host of service component.
check_comp_ets({Service, ServiceComp}) ->
	case ets:lookup(sys, {Service, ServiceComp}) of
		[Service, ServiceComp, Pid, Host] -> 
			{Service, ServiceComp, Pid, Host};
		[] -> {error, comp_not_found}
	end.



%% Universal interface for component communication
universal_interface() ->
	1.

%% Communication between components
comp_call({Service, ServiceComp, {Msg}}) ->
	[Service, ServiceComp, Pid, Host] = ?MODULE:check_comp_ets({Service,
																ServiceComp}).


%% Broadcast message to all nodes and process

%% Generate unique sequence id for each message, store in ets
%% epoch time + 2 digit random_number + 4 characters 
%% Assign 4 characters between integers.
sequence_id() ->
	%% nanoseconds
	{Mega, Sec, Micro} = os:timestamp(),
	Timeepoch = (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000),
	Randnum = rand:
	random:seed(erlang:phash2([node()]), erlang:monotonic_time(),
				erlang:unique_integer([monotonic])).

%% Get a Len length random string 
random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> 
				[element(rand:uniform(ChrsSize), Chrs) | R] 
		end,
    lists:foldl(F, "", lists:seq(1, Len)).



