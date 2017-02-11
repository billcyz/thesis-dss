%% @author billcyz
%% @doc @todo Add description to convert.


-module(convert).

%% ====================================================================
%% API functions
%% ====================================================================
-export([convert_ascii/1,
		 random_string/1,
		 sequence_id/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

sequence_id() ->
	%% nanoseconds
	{Mega, Sec, Micro} = os:timestamp(),
	(Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> 
				[element(rand:uniform(ChrsSize), Chrs) | R] 
		end,
    lists:foldl(F, "", lists:seq(1, Len)).

%% Convert ascii code
convert_ascii(A) ->
	case A of
		65 -> "A";
		66 -> "B";
		67 -> "C";
		68 -> "D";
		69 -> "E";
		70 -> "F";
		71 -> "G";
		72 -> "H";
		73 -> "I";
		74 -> "J";
		75 -> "K";
		76 -> "L";
		77 -> "M";
		78 -> "N";
		79 -> "O";
		80 -> "P";
		81 -> "Q";
		82 -> "R";
		83 -> "S";
		84 -> "T";
		85 -> "U";
		86 -> "V";
		87 -> "W";
		88 -> "X";
		89 -> "Y";
		90 -> "Z"
	end.
