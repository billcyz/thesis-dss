%% @author billcyz
%% @doc @todo Add description to traffic_handler.

%% Traffic handler provides APIs for applications developed in other languages.
%% It is the module which can be defined by developers.
%% The KNK framework provides APIs to applications. Those APIs provide ways to
%% create user-defined traffic handler, and handlers are duplicated/replicated
%% for distribution.

%% event manager? node? process?

-module(traffic_handler).
-behaviour(gen_server).

-export([]).

-record(state,
		{fnode,
		fcookie,
		 node,
		 cookie}).

%% ------------------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], 
						  []).

start_link(FNode_Info) ->
	{FNode, Cookie} = FNode_Info,
	gen_server:start_link({local, ?MODULE}, ?MODULE, 
						  [FNode, Cookie], []).

init(_Args) ->
	State = #state{fnode = [],
				   fcookie = [],
				   node = node(),
				   cookie = get_cookie()},
	io:format("~p (~p) starting...~n", [?MODULE, self()]),
	{ok, State}.

init(FNode, Cookie) ->
	State = #state{fnode = FNode,
				   fcookie = Cookie,
				   node = node(),
				   cookie = []},
	{ok, State}.

%% Send message
send_msg(Msg, Dst) ->
	1.

%% Encrypt message

%% Decrypt message