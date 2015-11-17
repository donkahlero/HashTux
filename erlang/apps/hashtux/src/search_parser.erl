-module(search_parser).

-export([parse_insta/1]).



parse_insta([]) 	-> ok;
parse_insta(List) ->
	case extract(<<"data">>, List) of
		{found, DataList} ->
			parse_ig_data(DataList);
		not_found					->
			not_found
	end.


parse_ig_data([]) -> [];
parse_ig_data([H|T]) ->
	Details = parse_ig_details(H),
	[Details | parse_ig_data(T)].



parse_ig_details([]) -> [];
parse_ig_details([{<<"tags">>, Value}|T])	->
	[{<<"tags">>, Value} | parse_ig_details(T)];
parse_ig_details([{<<"type">>, Value}|T]) ->
	[{<<"type">>, Value} | parse_ig_details(T)];
parse_ig_details([{<<"location">>, Value}|T]) ->
	[{<<"location">>, Value} | parse_ig_details(T)];
parse_ig_details([{<<"link">>, Value}|T]) ->
	[{<<"link">>, Value} | parse_ig_details(T)];
parse_ig_details([{<<"likes">>, Value}|T]) ->
	[{<<"likes">>, Value} | parse_ig_details(T)];
parse_ig_details([{<<"images">>, Value}|T]) ->
	[{_,_}, {_,_}, {<<"standard_resolution">>, V}] = Value,
	[{<<"images">>, V} | parse_ig_details(T)];
parse_ig_details([{_, _}|T]) ->
	parse_ig_details(T).



extract(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		{_, Value} -> {found, Value};
		false      -> not_found					  
	end.

