-module(ig_search).

-export([search/1]).


-define(ACCESS_TOKEN, "2253641420.43956b9.2ce7ad96329647ffa756501b24fbed3e").

-define(URL, "https://api.instagram.com/v1/tags/").
-define(TAIL, "/media/recent?count=2&access_token=").
-define(MEDIA, "Instagram").



search(Term) ->
	Url = ?URL ++ Term ++ ?TAIL ++ ?ACCESS_TOKEN,
	io:format("URL for IG: ~p~n", [Url]),
	case httpc:request(Url) of
		{ok, Result} -> 
			io:format("RESULT RECEIVED FROM IG~n"),
			{_StatusLine, _Headers, Body} = Result,
			DecodedRes = jsx:decode(list_to_binary(Body)),
			io:format("DECODED RESULT from IG: ~p~n", [DecodedRes]),
			parse(Term, DecodedRes);
		{error, Reason} ->
			io:format("REQUEST FAILED for reason: ~p~n", [Reason])
	end.



%%
%% @docParses the general result from a search.
%%
parse(_Term, []) 	-> [];
parse(Term, List) -> 
	case extract(<<"data">>, List) of
		{found, DataList} -> 
			parse_data(Term, DataList);
		not_found -> 
			not_found
	end.



%%
%% @docAppends details to single details results.
%%
append_details(_Term, []) 	-> [];
append_details(Term, List) ->
	Details = [{<<"search_term">>, list_to_binary(Term)}, 
 		  	  	 {<<"social_media">>, list_to_binary(?MEDIA)}],
	lists:append(List, Details).



%%
%% @docParses results from search query.
%%
parse_data(_Term, []) 	-> [];
parse_data(Term, [H|T]) ->
	Details = parse_details(H),
	NewDetails = append_details(Term, Details),
	[NewDetails | parse_data(Term, T)].



%%
%% @docParses details for one search result.
%%
parse_details([]) -> [];
%% tags 
parse_details([{<<"tags">>, Value}|T]) ->
	  [{<<"tags">>, Value} | parse_details(T)];
%% content type (eg image, video, etc.)
parse_details([{<<"type">>, Value}|T]) ->
	  [{<<"content_type">>, Value} | parse_details(T)];
%% location
parse_details([{<<"location">>, Value}|T]) when Value /= null ->
	  [{<<"location">>, Value} | parse_details(T)];
%% profile link
parse_details([{<<"link">>, Value}|T]) ->
	  [{<<"profile_link">>, Value} | parse_details(T)];
%% likes
parse_details([{<<"likes">>, [{<<"count">>, Value},
														 	{_,_}]}|T]) ->
	  [{<<"likes">>, Value} | parse_details(T)];
%% resource link
parse_details([{<<"images">>, Value}|T]) ->
	  [{_,_}, 
		 {_,_}, 
		 {<<"standard_resolution">>, [{<<"url">>, V}, 
																	{_, _}, 
																	{_, _}]}] = Value,
		[{<<"resourse_link">>, V} | parse_details(T)];
%% date source was created
parse_details([{<<"created_time">>, Value}|T]) ->
	  [{<<"date">>, Value} | parse_details(T)];
%% text 
parse_details([{<<"caption">>, [{_,_}, 
																{<<"text">>, Value},
															  {_,_},
															 	{_,_}]}|T]) ->
	  [{<<"text">>, Value} | parse_details(T)];
%% service id
parse_details([{<<"id">>, Value}|T]) ->
	  [{<<"service_id">>, Value} | parse_details(T)];
%% user details
parse_details([{<<"user">>, [{<<"username">>, Username},
														 {_,_},
														 {<<"id">>, Id},
														 {_,_}]}|T]) ->
	[{<<"username">>, Username}, 
	 {<<"user_id">>, Id} | parse_details(T)];
%% if nothing matches -> continue
parse_details([{_, _}|T]) ->
	  parse_details(T).



%%
%% @docExtracts a value from a list.
%%
extract(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		{_, TupleList} -> {found, TupleList};
		false 				 -> not_found
	end.
