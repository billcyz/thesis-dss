%% @author billcyz
%% @doc @todo Add description to knk_srv_sup.

%% Responsible for listening to socket specified by knk application.
%% Messages received from the socket will be sent to knk server, and send back the
%% reply to the initiator.


-module(knk_srv_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start/1,
		 start_socket/0]).

%% ----------------------------------------------------------------------------

%% start_link() ->
%% 	supervisor:start_link({local, ?MODULE}, ?MODULE, []),
%% 	ok.

start_port(Port) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%% init([]) ->
%% 	{ok, Port}, application:get_env(port),
%% 	{ok, LSocket} = gen_tcp:listen(Port, [binary, {active, once}]),
%% 	spawn_link(fun empty_listeners/0),
%% 	{ok, {{simple_one_for_one, 60, 6000},
%% 		  [{knk_srv,
%% 			{knk_srv, start_link, [LSocket]},
%% 		   permanent, 1000, worker, [knk_srv]}]}}.

%% Starts component supervisor 
init([]) ->
	case whereis(knk_comp_sup) of %% check existing component process
		undefined ->
			{ok, {{one_for_one, 60, 6000},
				  [{knk_comp_sup,
					{knk_comp_sup, start_link, []},
					transient, 1000, supervisor, [knk_comp_sup]}]}};
			%%ok = knk_sup:start_child_sup(knk_comp_sup, knk_comp_sup, []);
		Pid ->
			Pid ! stop,
			init([])
	end.
	

init(Port) ->
	%%{ok, Port}, application:get_env(port),
	{ok, LSocket} = gen_tcp:listen(Port, [binary, {active, once}]),
	spawn_link(fun empty_listeners/0),
	{ok, {{simple_one_for_one, 60, 6000},
		  [{knk_srv,
			{knk_srv, start_link, [LSocket]},
		   permanent, 1000, worker, [knk_srv]}]}}.

empty_listeners() ->
	[start_socket() || _ <- lists:seq(1, 20)].

start_socket() ->
	supervisor:start_child(?MODULE, []).



