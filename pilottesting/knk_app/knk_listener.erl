%% @author billcyz
%% @doc @todo Add description to knk_listener.


-module(knk_listener).
-behaviour(gen_tcp_server).
-export([]).

-export([start/2]).

-record(state, {}).

%% Start TCP listener for application
start(Port, Opts) ->
	gen_tcp_server:start_link(?MODULE, Port, Opts).

handle_tcp(Socket, Data, State) ->
	<<Msg/binary>> = Data,
	{ok, Request} = parse(binary_to_list(Msg)),
	case Request of
		{request, {App, M, F, A}} ->
			%% search App, M, F in knk.
			knk_comp_lookup:
	{ok, State}.

%% Parse data and get necessary information
parse(Msg) ->
	{ok, Token, _} = erl_scan:string(Msg ++ "."),
	{ok, Term} = erl_parse:parse_term(Token),
	Term.