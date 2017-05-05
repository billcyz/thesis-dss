%% @author billcyz
%% @doc @todo Add description to beta_app_sup.


-module(beta_app_sup).
-behaviour(supervisor).
-export([init/1]).


-export([start_link/0,
		 start_child_sup/1, start_child_sup/2, start_child_sup/3,
		 start_child/1, start_child/2, start_child/3, start_child/4,
		 stop_child/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ---------------------------------------------------------------------
%% Child supervisor
start_child_sup(Mod) ->
	start_child_sup(Mod, []).

start_child_sup(Mod, Args) ->
	start_child_sup(Mod, Mod, Args).

start_child_sup(ChildId, Mod, Args) ->
	child_reply(supervisor:start_child(?MODULE, 
									   {ChildId, {Mod, start_link, Args},
										transient, supervisor, [Mod]})).

%% ---------------------------------------------------------------------
%% Child process
start_child(Mod) ->
	start_child(Mod, []).

start_child(Mod, Args) ->
	start_child(Mod, Mod, Args).

start_child(ChildId, Mod, Args) ->
	child_reply(supervisor:start_child(?MODULE, 
									   {ChildId, {Mod, start_link, Args},
										transient, 60, worker, [Mod]})).

start_child(ChildId, Mod, Fun, Args) ->
	child_reply(supervisor:start_child(?MODULE,
									   {ChildId, {Mod, Fun, Args},
										transient, 60, worker, [Mod]})).

stop_child(ChildId) ->
	case supervisor:terminate_child(?MODULE, ChildId) of
		ok -> supervisor:delete_child(?MODULE, ChildId);
		E -> E
	end.

%% start_main_sup() ->
%% 	Port = application:get_env(beta_app, port),
%% 	LogFile = application:get_env(beta_app, log),
%% 	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, LogFile]).

init([]) ->
	{ok, {{one_for_all, 0, 1}, []}}.

child_reply({ok, _Pid}) -> ok;

child_reply(X) -> X.