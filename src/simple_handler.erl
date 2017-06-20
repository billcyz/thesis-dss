%% @author billcyz
%% @doc @todo Add description to simple_handler.


-module(simple_handler).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([]).

-record(state, {sys}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	create_ets(),
	{ok, #state{sys = node()}}.

create_ets() ->
	ets:new(comp, [named_table]).

%% CompInfo -> {CompName, [FunName]}
insert_data(CompInfo) ->
	ets:insert(comp, CompInfo).


%% prepare() ->
%% 	{download,
%% 	 [{args, [username, url]},
%% 	  {requires, [verify_user(username)]}]}.

require_comp(RequestNode, CompName, FunInfo, Data) ->
	gen_server:call(?MODULE, {require_comp, RequestNode, CompName, FunInfo,
							  Data}).

find_comp(CompName, FuncInfo) ->
	gen_server:call(?MODULE, {find_comp, CompName, FuncInfo}).

handle_call({find_comp, CompName, FuncInfo}, _From, State) ->
	case ets:lookup(comp, CompName) of
		[] -> false;
		[{CompName, CompNode, FunList}] ->
			case lists:member(FuncInfo, FunList) of
				true -> {reply, {ok, CompNode}, State};
				false -> {reply, false, State}
			end
	end;
handle_call({require_comp, RequestNode, CompName, FuncInfo, Data}, _From, 
			State) ->
	case find_comp(CompName, FuncInfo) of
		{ok, CompNode} -> 
			{CompName, CompNode} ! {app_traffic, RequestNode, FuncInfo, Data};
		false ->
			io:format("Component is found in local storage~n")
			%find_global_comp(CompName, FuncName)
	end,
	{noreply, State}.


handle_info({{register, ModuleName, CompInfo}, CompNode}, State) ->
	insert_data(CompInfo),
	{ModuleName, CompNode} ! {register_success, node()},
	{noreply, State}.





