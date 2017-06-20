%% @author billcyz
%% @doc @todo Add description to knk_comp_registry.

%% KNK component registration

-module(knk_comp_registry).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).

-define(SERVER, ?MODULE).

%% ----------------------------------------------------------------------------------



