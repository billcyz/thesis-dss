%% @author billcyz
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add/0, show/0]).
-export([test/1, parse_str/1]).

-export([change_data/1, parse/1]).

-export([start_proc/0, start_loop/0]).

-compile(export_all).

%% ====================================================================
%% Internal functions
%% ====================================================================

-define(PORT, add()).

add() ->
	1000.

show() ->
	?PORT.

test(X) ->
	list_to_string(X).

list_to_string(X) ->
	{ok, T, _} = erl_scan:string(X++"."),
	case erl_parse:parse_term(T) of
		{ok, Term} ->
			Term;
		{error, Error} ->
            Error
    end.

parse_str(Input) ->
	"{" ++ Tail = Input,
	case Tail of
		"request, " ++ MFT -> [request, strip(MFT)]
	end.

strip(Input) ->
	lists:reverse(tl(lists:reverse(Input))).


change_data(Data) ->
	%% asdasdasd
	%% asdasdasdasd
	NewData = <<"okokokokokokokokokkookokok", Data/binary>>,
	NewData.


parse(Msg) ->
	{ok, Token, _} = erl_scan:string(Msg ++ "."),
	{ok, Term} = erl_parse:parse_term(Token),
	Term.

start_proc() ->
	register(test_process, spawn(?MODULE, start_loop, [])).

start_loop() ->
	loop().

loop() ->
	receive
		1 -> io:format("One~n"),
			 loop()
	end.


command_line() ->
	c:ls().


create_tab(TabList) ->
	lists:map(fun(Tab) ->
					  ets:new(Tab, [named_table]) end, TabList),
	ok.

check_new_srv(TabList) ->
	case lists:member(1, lists:map(fun(Tab) -> 
					  ets:info(Tab, size) end, TabList)) of
		true -> not_new;
		false -> new
	end.



create_count_table() ->
	ets:new(dist_count, [private, named_table]),
	case ets:info(dist_count) of
		undefined -> {error, count_tab_failed};
		_ -> ok
	end.

update_count(CompL) ->
	lists:map(
	  fun(CompName) -> 
			  [{_, Count}] = ets:lookup(dist_count, CompName), 
			  ets:insert(dist_count, {CompName, Count + 1}) end, 
	  CompL),
	ok.








