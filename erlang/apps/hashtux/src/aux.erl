-module(aux).

-export([bin_to_atom/1]).



bin_to_atom(Binary) ->
	list_to_atom(binary_to_list(Binary)).



