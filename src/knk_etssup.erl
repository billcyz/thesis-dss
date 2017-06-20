%% @author billcyz
%% @doc @todo Add description to knk_etssup.


-module(knk_etssup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/2]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% --------------------------------------------------------------------------

%% Starts ets supervisor which relates to KNK system.
%% KNKName is the name of KNK system node
start_link(KNKName, AppName) ->
	supervisor:start_link({lcoal, ?MODULE}, ?MODULE, [KNKName, AppName]).

init([KNKName, AppName]) ->
	{links, [Parent]} = process_info(self(), links),
	unlink(Parent),
	Child1 = ?CHILD(knk_etssrv, [KNKName, AppName], worker),
	Child2 = ?CHILD(knk_etsmgr, [KNKName, AppName], worker),
	{ok, { {one_for_one, 5, 10}, [Child1, Child2]} }.


