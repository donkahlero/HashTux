-module(aux).

-export([bin_to_atom/1]).



bin_to_atom(Binary) ->
	list_to_atom(binary_to_list(Binary)).

get_value(_Key, [])	  -> [];
get_value(_Key, null) -> [];
get_value(Key, List)  ->
	case lists:keyfind(Key, 1, List) of
		{_K, V}	-> V;
		false 	-> []
	end.

