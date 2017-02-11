%% @author billcyz

-module(knk_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_main_sup/0,
		 start_child/1, start_child/2, start_child/3, start_child/4,
		 start_sup_child/1, start_sup_child/2, start_sup_child/3,
		 stop_child/1]).

-include("knk.hrl").

%% --------------------------------------------------------------------

-define(SERVER, ?MODULE).

%% ---------------------------------------s-----------------------------

%% Start main supervisor
start_main_sup() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Start child process
start_child(Mod) -> start_child(Mod, []).

start_child(Mod, Args) -> start_child(Mod, Mod, Args).

start_child(ChildId, Mod, Args) ->
	child_reply(supervisor:start_child(
				  ?SERVER,
				  {ChildId, {Mod, start_link, Args},
				   transient, ?WORKER_WAIT, worker, [Mod]})).

start_child(ChildId, Mod, Fun, Args) ->
	child_reply(supervisor:start_child(
				  ?SERVER, 
				  {ChildId, {Mod, Fun, Args},
				   transient, ?WORKER_WAIT, worker, [Mod]})).

%% Start child supervisor
start_sup_child(Mod) -> start_sup_child(Mod, []).

start_sup_child(Mod, Args) -> start_sup_child(Mod, Mod, Args).

start_sup_child(ChildId, Mod, Args) ->
	child_reply(supervisor:start_child(
				  ?SERVER, 
				  {ChildId, {Mod, start_link, Args},
				   transient, infinity, supervisor, [Mod]})).

%% Stop child process
stop_child(ChildId) ->
	case supervisor:terminate_child(?SERVER, ChildId) of
		ok -> supervisor:delete_child(?SERVER, ChildId);
		E -> E
	end.

init([]) ->
	_SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
	_ChildSpecs = [#{id => knk_sup,
					start => {beta_sup, start_link, []},
					restart => permanent,
					shutdown => 100000,
					type => supervisor,
					modules => [beta_sup]}],
	
	SupFlags = {one_for_one, 1, 5},
	ChildSpecs = [{knk_main_sup, %% main supervisor
				   {knk_main_sup, start_link, []},
				   permanent,
				   infinity,
				   supervisor,
				   [knk_main_sup]},
				  {knk_log_sup, %% logging
				   {knk_log_sup, start, []},
				   permanent,
				   infinity,
				   supervisor,
				   [knk_log_sup]}],
	{ok, {SupFlags, ChildSpecs}}.


%%----------------------------------------------------------------------------

child_reply({ok, _}) -> ok;
child_reply(X)       -> X.


