%% @author billcyz
%% @doc @todo Add description to temp.



-module(temp).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([list_children/0]).

start_link() ->
        supervisor:start_link({local, ?MODULE}, ?MODULE, []),
        supervisor:start_child(?MODULE, [{temp_sup, {temp_sup, start_link, []}, permanent, infinity, supervisor, [temp_sup]}]).

list_children() ->
        supervisor:which_children(?MODULE).

init([]) ->
        io:format("****Main supervisor****~n"),
        {ok, {{one_for_one, 60, 3600}, [{temp_srv, {temp_srv, start_link, []}, permanent, 1000, worker, [temp_srv]}]}}.


