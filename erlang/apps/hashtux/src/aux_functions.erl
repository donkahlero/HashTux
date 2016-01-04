%
% @doc This module contains some helper functions that are seen as useful throughout the application.
%
-module(aux_functions).

-export([bin_to_atom/1, get_value/2, ignore_request_type/1]).




%%
%% @doc Converts binary term to atom.
%%
bin_to_atom(Binary) ->
	list_to_atom(binary_to_list(Binary)).


%%
%% @doc Returns the value from a {key, value} pair in a list.
%%
get_value(_Key, [])	  -> [];
get_value(_Key, null) -> [];
get_value(Key, List)  ->
	case lists:keyfind(Key, 1, List) of
		{_K, V}	-> V;
		false 	-> []
	end.
	

%%
%% @doc Strip away request_type field from the options list
%% Uses keytake - in TupleList2 the first occurrence of Key is removed
%% keytake(Key, N, TupleList1) -> {value, Tuple, TupleList2} | false
%%
ignore_request_type(Options) ->
	X = lists:keytake(request_type, 1, Options),
	case X of 		
		{_Value, _Tuple, Options2} ->
			% The key request_type was present. Return the remaining options
			Options2;
		false ->
			% The key was not present. Return unchanged options.
			Options
	end.
