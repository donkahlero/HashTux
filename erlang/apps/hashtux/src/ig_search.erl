-module(ig_search).

-export([search/2]).

-define(URL, "https://api.instagram.com/v1/tags/").
-define(TAIL, "/media/recent?count=50&access_token=").
-define(MEDIA, "instagram").




search(Term, Options) ->
	Token = get_token(),
	Url = ?URL ++ Term ++ ?TAIL ++ Token,
	case httpc:request(Url) of
		{ok, Result} -> 
			{_StatusLine, _Headers, Body} = Result,
			try jsx:decode(list_to_binary(Body)) of
				DecodedRes -> 
					DataList = get_value(<<"data">>, DecodedRes),
					Results = parse_results(Term, DataList),
					L = get_value(content_type, Options),
					case Results of 
						[] ->
							io:format("NOT WRITING TO DB~n"), 
							ok;
						R  ->
							gen_server:call(db_serv, {add_doc, Results})
					end,
					filter_insta(Results, L)
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
	Key = case get_value(access_token, Account) of
				[] -> [];
				V  -> V
		  end,
	Key.


%% 
%% @doc Checks for the options for which to filter Instagram results. The
%% options can be 'image' and 'video'. Calls filter_insta_res/2 if needed.
%% 
filter_insta([], _L)  -> [];
filter_insta(Res, []) -> Res;
filter_insta(Res, L)  ->
	case {lists:member(<<"image">>, L), lists:member(<<"video">>, L)} of
		{true, true}   -> Res;
 		{true, false}  -> filter_insta_res(Res, <<"image">>);
		{false, true}  -> filter_insta_res(Res, <<"video">>)
	end.


%%
%% @doc Filters the results returned from Instagram based on the key 
%% passed. The key is an atom. Returns a list containing the results 
%% for which the key matches the key atom returned from get_val_atom/1. 
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
parse_results(_Term, []) 	-> [];
parse_results(Term, [X|Xs]) ->
	[ parse_details(Term, X) | parse_results(Term, Xs) ].


%%
parse_details(_Term, []) -> [];
parse_details(Term, L)	 -> 
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
get_search_term(Term) ->
	{<<"search_term">>, list_to_binary(Term)}.


%%
get_service() ->
	{<<"service">>, list_to_binary(?MEDIA)}.


%%
get_timestamp() ->
	{<<"insert_timestamp">>, dateconv:get_timestamp()}.


%%
get_tags([]) -> [];
get_tags(L)  ->
	{<<"tags">>, get_value(<<"tags">>, L)}.


%%
get_content_type([]) -> [];
get_content_type(L)  ->
	{<<"content_type">>, get_value(<<"type">>, L)}.


%%
get_location([]) -> [];
get_location(L)	 ->
	V = case get_value(<<"location">>, L) of
			null -> [];
			XY	 -> XY
		end,
	{<<"location">>, V}.


%%
get_profile_link([]) -> [];
get_profile_link(L)	 ->
	Username = get_value(<<"username">>, get_value(<<"user">>, L)),
	Url = "https://instagram.com/",
	Value = list_to_binary(lists:append(Url, binary:bin_to_list(Username))),
	{<<"profile_link">>, Value}.


%%
get_likes([]) -> [];
get_likes(L)  ->
	LikesData = get_value(<<"likes">>, L),
	{<<"likes">>, get_value(<<"count">>, LikesData)}.


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
get_res_link_low([]) -> [];
get_res_link_low(L)  ->
	case get_value(<<"type">>, L) of
		<<"image">> -> 
			get_img_link_low(L);
		<<"video">> -> 
			get_vid_link_low(L)
	end.


%%
get_img_link_high([]) -> [];
get_img_link_high(L)  ->
	ImageData = get_value(<<"images">>, L),
	Resources = get_value(<<"standard_resolution">>, ImageData),
	{<<"resource_link_high">>, get_value(<<"url">>, Resources)}.


%%
get_img_link_low([]) -> [];
get_img_link_low(L)  ->
	ImageData = get_value(<<"images">>, L),
	Resources = get_value(<<"low_resolution">>, ImageData),
	{<<"resource_link_low">>, get_value(<<"url">>, Resources)}.


%%
get_vid_link_high([]) -> [];
get_vid_link_high(L)  ->
	VideoData = get_value(<<"videos">>, L),
	Resources = get_value(<<"standard_resolution">>, VideoData),
	{<<"resource_link_high">>, get_value(<<"url">>, Resources)}.


%%
get_vid_link_low([]) -> [];
get_vid_link_low(L)  ->
	VideoData = get_value(<<"videos">>, L),
	Resources = get_value(<<"low_resolution">>, VideoData),
	{<<"resource_link_low">>, get_value(<<"url">>, Resources)}.


%%
get_created_time([]) -> [];
get_created_time(L)  ->
	{<<"timestamp">>, 
	 list_to_integer(binary:bin_to_list(get_value(<<"created_time">>, L)))}.


%% 
get_text([]) -> [];
get_text(L)  ->
	CaptionData = get_value(<<"caption">>, L),
	{<<"text">>, get_value(<<"text">>, CaptionData)}.


%%
get_service_id([]) -> [];
get_service_id(L)  ->
	{<<"service_id">>, get_value(<<"id">>, L)}.


%%
get_username([]) -> [];
get_username(L)  ->
	UserInfo = get_value(<<"user">>, L),
	{<<"username">>, get_value(<<"username">>, UserInfo)}.


%%
get_user_id([]) -> [];
get_user_id(L)  ->
	UserInfo = get_value(<<"user">>, L),
	{<<"user_id">>, get_value(<<"id">>, UserInfo)}.
	
 

