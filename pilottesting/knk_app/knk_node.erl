%% @author billcyz
%% @doc @todo Add description to knk_node.


-module(knk_node).

-export([]).

%% -----------------------------------------------------------------------

%% knk_node callback definition
-callback handle_broadcast(NodeL :: list(), MFA :: list(), State :: term()) ->
	{ok, NewState :: term()} | {stop, Reason :: term()}.

-callback handle_unicast(Node :: list(), MFA :: list(), State :: term()) ->
	{ok, NewState :: term()} | {stop, Reason :: term()}.

-callback handle_block(Node :: list(), State :: term()) ->
	{ok, NewState :: term()} | {stop, Reason :: term()}.

-callback handle_shadow(Node :: list(), State :: term()) ->
	ok.

-callback terminate(Node :: list(), State :: term()) ->
	ok.

%% Start knk_node with default option
start_link(HandlerModule) ->
	start_link(HandlerModule, start_link, []).

%% Start knk_node with user defined options
start_link(HandlerModule, Func, Args) ->
	knk_node_sup:start_link(HandlerModule, Func, Args).