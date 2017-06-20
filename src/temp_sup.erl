%% @author billcyz
%% @doc @todo Add description to temp_sup.


-module(temp_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/0, list_children/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

list_children() ->
	supervisor:which_children(?MODULE).

init([]) ->
	io:format("****Temp supervisor*****~n"),
	spawn_link(fun start_pool/0),
	{ok, {{simple_one_for_one, 0, 1}, 
		  [{temp_worker, {temp_worker, start_link, []}, 
			temporary, 1000, worker, [temp_worker]}]}}.

start_pool() ->
	[start_temp_worker() || _ <- lists:seq(1, 3)].

start_temp_worker() ->
	supervisor:start_child(?MODULE, []).
