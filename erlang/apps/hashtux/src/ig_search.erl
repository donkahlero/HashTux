-module(ig_search).

-export([search/2]).

-define(URL, "https://api.instagram.com/v1/tags/").
-define(TAIL, "/media/recent?count=40").
-define(MIN_TIME, "min_timestamp=").
-define(MAX_TIME, "max_timestamp=").
-define(ACCESS, "access_token=").
-define(MEDIA, "instagram").
-define(AND, "&").




%%
%% @doc Searches Instagram for the term passed and filters the results
%% according to the options passed.
%%
search(Term, Options) ->
	%Token = get_token(),
	%Url = ?URL ++ Term ++ ?TAIL ++ Token,
	FormattedTerm = apis_aux:format_keyword(Term),
	Url = build_request(FormattedTerm, Options),
	case httpc:request(Url) of
		{ok, Result} -> 
			{_StatusLine, _Headers, Body} = Result,
			try jsx:decode(list_to_binary(Body)) of
				DecodedRes -> 
					DataList = get_value(<<"data">>, DecodedRes),
					%io:format("Raw results are: ~p~n", [DecodedRes]),
					Results = parse_results(Term, DataList),
					ResLength = length(Results),
					io:format("IG_SEARCH: Unfiltered result count: ~p~n", [ResLength]),
					Types = get_value(content_type, Options),
					FilterRes = filter_insta(Results, Types),
					FilterResLength = length(FilterRes),
					io:format("IG_SEARCH: Filtered result count: ~p~n", [FilterResLength]),
					[{filtered, Results}, {unfiltered, FilterRes}]
			catch _ -> []
			end;
		{error, Reason} ->
			io:format("IG_SEARCH: Request failed for reason: ~p~n", [Reason]),
			[]
	end.


%%
%% @doc Builds the Url for a request.
%%
build_request(Term, Options) ->
	Token = get_token(),
	case get_value(history_timestamp, Options) of
		[] -> 
			Url1 = ?URL ++ Term ++ ?TAIL ++ ?AND ++ ?ACCESS ++ Token,
			%io:format("MINER_WORKER: Build Url: ~p~n", [Url1]),
			Url1;
		Value ->
			MinTime = Value - 43200,
			MaxTime = get_max_time(Value),
			Url2 = ?URL ++ Term ++ ?TAIL ++ ?AND ++ ?MIN_TIME ++ 
					erlang:integer_to_list(MinTime) ++ ?AND ++ ?MAX_TIME ++ 
					erlang:integer_to_list(MaxTime) ++ ?AND ++ ?ACCESS ++ Token,
			%io:format("MINER_WORKER: Build Url: ~p~n", [Url2]),
			Url2
	end.


%%
%% @doc Returns the max timestamp for a search.
%%
get_max_time(Time) ->
	{_, Secs, _} = os:timestamp(),
	NewTime = Time + 43200,
	if 
		NewTime > Secs -> Secs;
		NewTime =< Secs -> NewTime
	end.
		

%%
%% @docGets the access token for instagram.
%%
get_token() ->
	{ok, Token} = application:get_env(instagram_account, access_token),
	Token.
%	Key = case get_value(access_token, Account) of
%				[] -> [];
%				V  -> V
%		  end,
%	Key.


%% 
%% @doc Checks for the options for which to filter Instagram results. The
%% options can be 'image' and 'video'. Calls filter_insta_res/2 if needed.
%% 
filter_insta([], _L)  -> [];
filter_insta(Res, []) -> Res;
filter_insta(Res, L)  ->
	case {lists:member(<<"image">>, L), lists:member(<<"video">>, L)} of
		{true, true}   -> Res;
		{false, false} -> Res;
 		{true, false}  -> filter_insta_res(Res, <<"image">>);
		{false, true}  -> filter_insta_res(Res, <<"video">>)
	end.


%%
%% @doc Filters the results returned from Instagram based on the key 
%% passed. 
%% 
filter_insta_res([], _Key)	 -> [];
filter_insta_res(List, Key) ->
	[N || N <- List, get_value(<<"content_type">>, N) == Key]. 


%%
%% @doc Gets the value from the key-value pair with key content_type
%% in the results from Instagram. Returns this value name as atom or 
%% the atom 'no_atom' if not found.
%%
%get_val_atom(Key, List) ->
%	X = case lists:keyfind(Key, 1, List) of
%			{_K, V} -> list_to_atom(binary_to_list(V));
%			false	-> no_atom
%		end,
%	X.


%%
get_value(_Key, [])	  -> [];
get_value(_Key, null) -> [];
get_value(Key, List)  ->
	case lists:keyfind(Key, 1, List) of
		{_K, V}	-> V;
		false 	-> []
	end.


%%
%% @doc Parses the individual result from the data query.
%%
parse_results(_Term, [])	-> [];
parse_results(Term, [X|Xs]) ->
	[ parse_details(Term, X) | parse_results(Term, Xs) ].


%%
%% @doc Parses the details of an individual result.
%%
parse_details(_Term, []) -> [];
parse_details(Term, L)   -> 
	[ get_search_term(Term),
	  get_service(),
	  get_timestamp(),
	  get_tags(L), 
	  get_content_type(L),
	  get_location(L),
	  get_profile_link(L), 
	  get_likes(L), 
	  get_res_link_high(L),
	  get_res_link_low(L),
	  get_created_time(L),
	  get_text(L),
	  get_service_id(L),
	  get_username(L),
	  get_user_id(L) ].	
	

%%
%% @doc Returns the search term {key, value} pair.
%%
get_search_term(Term) ->
	{<<"search_term">>, list_to_binary(Term)}.


%%
%% @doc Returns the service {key, value} pair.
%%
get_service() ->
	{<<"service">>, list_to_binary(?MEDIA)}.


%%
%% @doc Returns the insert timestamp {key, value} pair.
%%
get_timestamp() ->
	{<<"insert_timestamp">>, dateconv:get_timestamp()}.


%%
%% @doc Returns the next max tag id {key, value} pair.
%%
%%get_tag_id(TagId) ->
%%	{<<"tag_id">>, TagId}.	


%%
%% @doc Returns the tags {key, value} pair.
%%
get_tags([]) -> [];
get_tags(L)  ->
	{<<"tags">>, get_value(<<"tags">>, L)}.


%%
%% @doc Returns the content type {key, value} pair.
%%
get_content_type([]) -> [];
get_content_type(L)  ->
	{<<"content_type">>, get_value(<<"type">>, L)}.


%%
%% @doc Returns the location {key, value} pair.
%%
get_location([]) -> [];
get_location(L)	 ->
	V = case get_value(<<"location">>, L) of
			null -> [];
			XY	 -> XY
		end,
	{<<"location">>, V}.


%%
%% @doc Returns the profile link {key, value} pair.
%%
get_profile_link([]) -> [];
get_profile_link(L)	 ->
	Username = get_value(<<"username">>, get_value(<<"user">>, L)),
	Url = "https://instagram.com/",
	Value = list_to_binary(lists:append(Url, binary:bin_to_list(Username))),
	{<<"profile_link">>, Value}.


%%
%% @doc Returns the likes {key, value} pair.
%%
get_likes([]) -> [];
get_likes(L)  ->
	LikesData = get_value(<<"likes">>, L),
	{<<"likes">>, get_value(<<"count">>, LikesData)}.


%%
%% @doc Returns the resource link high {key, value} pair.
%%
get_res_link_high([]) -> [];
get_res_link_high(L)  ->
	case get_value(<<"type">>, L) of
		<<"image">> -> 
			get_img_link_high(L);
		<<"video">> -> 
			get_vid_link_high(L)
	end.


%%
%% @doc Returns the resource link low {key, value} pair.
%%
get_res_link_low([]) -> [];
get_res_link_low(L)  ->
	case get_value(<<"type">>, L) of
		<<"image">> -> 
			get_img_link_low(L);
		<<"video">> -> 
			get_vid_link_low(L)
	end.


%%
%% @doc Returns the image resource link high {key, value} pair.
%%
get_img_link_high([]) -> [];
get_img_link_high(L)  ->
	ImageData = get_value(<<"images">>, L),
	Resources = get_value(<<"standard_resolution">>, ImageData),
	{<<"resource_link_high">>, get_value(<<"url">>, Resources)}.


%%
%% @doc Returns the image resource link low {key, value} pair.
%%
get_img_link_low([]) -> [];
get_img_link_low(L)  ->
	ImageData = get_value(<<"images">>, L),
	Resources = get_value(<<"low_resolution">>, ImageData),
	{<<"resource_link_low">>, get_value(<<"url">>, Resources)}.


%%
%% @doc Returns the video resource link high {key, value} pair.
%%
get_vid_link_high([]) -> [];
get_vid_link_high(L)  ->
	VideoData = get_value(<<"videos">>, L),
	Resources = get_value(<<"standard_resolution">>, VideoData),
	{<<"resource_link_high">>, get_value(<<"url">>, Resources)}.


%%
%% @doc Returns the video resource link low {key, value} pair.
%%
get_vid_link_low([]) -> [];
get_vid_link_low(L)  ->
	VideoData = get_value(<<"videos">>, L),
	Resources = get_value(<<"low_resolution">>, VideoData),
	{<<"resource_link_low">>, get_value(<<"url">>, Resources)}.


%%
%% @doc Returns the timestamp {key, value} pair.
%%
get_created_time([]) -> [];
get_created_time(L)  ->
	{<<"timestamp">>, 
	 list_to_integer(binary:bin_to_list(get_value(<<"created_time">>, L)))}.


%% 
%% @doc Returns the text {key, value} pair.
%%
get_text([]) -> [];
get_text(L)  ->
	CaptionData = get_value(<<"caption">>, L),
	{<<"text">>, get_value(<<"text">>, CaptionData)}.


%%
%% @doc Returns the service id {key, value} pair.
%%
get_service_id([]) -> [];
get_service_id(L)  ->
	{<<"service_id">>, get_value(<<"id">>, L)}.


%%
%% @doc Returns the username {key, value} pair.
%%
get_username([]) -> [];
get_username(L)  ->
	UserInfo = get_value(<<"user">>, L),
	{<<"username">>, get_value(<<"username">>, UserInfo)}.


%%
%% @doc Returns the user id {key, value} pair.
%%
get_user_id([]) -> [];
get_user_id(L)  ->
	UserInfo = get_value(<<"user">>, L),
	{<<"user_id">>, get_value(<<"id">>, UserInfo)}.
	
 





