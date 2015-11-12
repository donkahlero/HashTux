-module(ig_search).

-export([search/1]).

-define(URL, "https://api.instagram.com/v1/tags/").
-define(TAIL, "/media/recent?access_token=").
-define(MEDIA, "instagram").




search(Term) ->
	Token = get_token(),
	Url = ?URL ++ Term ++ ?TAIL ++ Token,
	case httpc:request(Url) of
		{ok, Result} -> 
			{_StatusLine, _Headers, Body} = Result,
			try jsx:decode(list_to_binary(Body)) of
				DecodedRes -> 
					Results = parse_data(Term, DecodedRes),
					gen_server:call(db_serv, {add_doc, Results}),
					Results
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
parse_data(_Term, []) -> [];
parse_data(Term, L)	  ->
	DataList = get_value(<<"data">>, L),
	parse_results(Term, DataList).


%%
parse_results(_Term, []) 	-> [];
parse_results(Term, [X|Xs]) ->
	[parse_details(Term, X) | parse_results(Term, Xs)].


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
	  get_vid_link_high(L), 
	  get_vid_link_low(L),
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
	UserInfo = get_value(<<"user">>, L),
	Username = get_value(<<"username">>, UserInfo),
	Url = "https://instagram.com/",
	{<<"profile_link">>, lists:append(Url, binary:bin_to_list(Username))}.


%%
get_likes([]) -> [];
get_likes(L)  ->
	LikesData = get_value(<<"likes">>, L),
	{<<"likes">>, get_value(<<"count">>, LikesData)}.


%%
get_res_link_high([]) -> [];
get_res_link_high(L)  ->
	ImageData = get_value(<<"images">>, L),
	Resources = get_value(<<"standard_resolution">>, ImageData),
	{<<"resource_link_high">>, get_value(<<"url">>, Resources)}.


%%
get_res_link_low([]) -> [];
get_res_link_low(L)  ->
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
	
 
%%
get_value(_Key, [])  -> [];
get_value(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		{_K, V}	-> V;
		false 	-> []
	end.







