-module(ig_search).

-export([search/2]).

-define(URL, "https://api.instagram.com/v1/tags/").
-define(TAIL, "/media/recent?access_token=").
-define(MEDIA, "Instagram").



search(Term, _Options) ->
	Token = get_token(),
	Url = ?URL ++ Term ++ ?TAIL ++ Token,
	case httpc:request(Url) of
		{ok, Result} -> 
			{_StatusLine, _Headers, Body} = Result,
			try jsx:decode(list_to_binary(Body)) of
				DecodedRes -> parse(Term, DecodedRes)
			catch _ -> []
			end;
		{error, Reason} ->
			io:format("REQUEST FAILED for reason: ~p~n", [Reason]),
			[]
	end.



%%
%% @docGets the access token for instagram.
%%
get_token() ->
	{ok, Account} = application:get_env(hashtux, instagram_account),
	Key = case extract(access_token, Account) of
					 	{found, K} -> K;
						not_found  -> []
				end,
	Key.



%%
%% @docParses the general result from a search.
%%
parse(_Term, []) 	-> [];
parse(Term, List) -> 
	case extract(<<"data">>, List) of
		{found, DataList} -> 
			parse_data(Term, DataList);
		not_found -> []
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
%% resource link for images
parse_details([{<<"images">>, Value}|T]) ->
	  [{<<"low_resolution">>, [{<<"url">>, V1},
													 {_, _},
													 {_, _}]}, 
		 {_,_}, 
		 {<<"standard_resolution">>, [{<<"url">>, V2}, 
																	{_, _}, 
																	{_, _}]}] = Value,
		[{<<"resource_link_high">>, V2},
		 {<<"resource_link_low">>, V1} | parse_details(T)];
%% resource link for videos
parse_details([{<<"videos">>, Value}|T]) ->
	  [{_,_}, 
		 {<<"standard_resolution">>, [{<<"url">>, V1},
																 	{_, _},
																 	{_, _}]}, 
		 {<<"low_resolution">>, [{<<"url">>, V2}, 
														 {_, _}, 
														 {_, _}]}] = Value,
		[{<<"resource_link_high">>, V1},
		 {<<"resource_link_low">>, V2} | parse_details(T)];
%% date source was created
parse_details([{<<"created_time">>, Value}|T]) ->
	  [{<<"timestamp">>, Value} | parse_details(T)];
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
