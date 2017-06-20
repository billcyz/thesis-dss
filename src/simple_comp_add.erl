%% @author billcyz
%% @doc @todo Add description to simple_comp.


-module(simple_comp_add).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, send_info/2, register/1, add/2]).

-record(state, {server}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

require_fun(ServerNode, CompName, FunInfo, Data) ->
	rpc:call(ServerNode, simple_handler, require_comp, 
			 [node(), CompName, FunInfo, Data]).

init([]) ->
	{ok, #state{}}.

register(ServerNode) ->
	{simple_handler, ServerNode} ! {{register, ?MODULE,
									 prepare_comp_info()}, node()}.

%% Prepare component information
prepare_comp_info() ->
	{?MODULE, node(), [{add, 2}]}.

add(X, Y) ->
	gen_server:call(?MODULE, {add, X, Y}).

send_info(Node, Msg) ->
	{?MODULE, Node} ! {node(), Msg}.

send_result_info(Node, Msg) ->
	{?MODULE, Node} ! {node(), result, Msg}.

handle_info({register_success, ServerNode}, State) ->
	io:format("Register success, server is ~p~n", [ServerNode]),
	{noreply, State#state{server = ServerNode}};
handle_info({app_traffic, RequestNode, FuncInfo, Data}, State) ->
	if
		FuncInfo =:= {add, 2} ->
			[X, Y] = Data,
			Result = add(X, Y),
			{RequestNode} ! {app_traffic_result, Result};
		true -> 1
	end,
	{noreply, State}.

handle_call({add, X, Y}, _From, State) ->
	{reply, X + Y, State}.

handle_cast(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


