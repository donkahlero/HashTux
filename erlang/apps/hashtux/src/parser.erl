-module(parser).

-export([extract/2, atom_to_binarystring/1]).
-export([extract_youtube_ids/1, parse_youtube_video/2, clean_result/1, is_language/2]).						% YOUTUBE Functions
-export([filter_by_content_type/2, parse_tweet_response_body/2]).																				% TWITTER Functions

% *****************************************************************************
% @doc A parser for decoded JSON Items
%       Handle conversion from decoded FEEDS to internal representation format
% *****************************************************************************

% the value of a key from decoded JSON message.
extract(K, L) ->
  case lists:keyfind(K, 1, L) of
    {_, M} -> {found, M};
    false  -> not_found
  end.

% @doc extract a field from a parent node. 
% return value if field was found, null if it was not found 
extract_from_node(_Field, null) -> null;
extract_from_node(Field, Node) -> 
	case extract(Field, Node) of
		{found, X} -> X; 
		not_found -> null
    end.

%%
%% Remove empty fields from parsed item (i.e. removes fields that returned null)
%%
clean_result(L) -> [X || X <- L, has_null_value(X) == false].

has_null_value({_, null}) -> true;
has_null_value({_, _}) -> false.

%%
%% @doc Convert an atom to binary_string
%%
atom_to_binarystring(Atom) ->
    list_to_binary(atom_to_list(Atom)).

% ****************************************
% PARSE YOUTUBE decoded JSON
% ****************************************

% @ doc extract the Id from a list of decoded Youtube feeds returned by a Keyword search 
extract_youtube_ids(List) ->
	extract_ids(List, []).

extract_ids([], Result) -> Result;
extract_ids([H|T], Result) ->
	NewResult = Result ++ [extract_id(H)],
	extract_ids(T, NewResult).

extract_id([{<<"id">>, [{<<"videoId">>, VideoId}]}]) -> binary_to_list(VideoId);
extract_id(_) -> parser_error.

% Builds a Youtube Channel URL for a given channelId
build_youtube_channel_link(null) -> null;
build_youtube_channel_link(ChannelId) -> 
    A = lists:append("https://youtube.com/channel/", binary_to_list(ChannelId)),
    list_to_binary(A).

% Builds a Youtube Video URL for a given videoId
build_youtube_video_link(null) -> null;
build_youtube_video_link(VideoId) -> 
    A = lists:append("https://www.youtube.com/watch?v=", binary_to_list(VideoId)),
    list_to_binary(A).


%%
%% @doc convert a decoded youtube Video resource to internal representation
%%
parse_youtube_video(Video, HashTag) -> 
	
	%% Register the time the document was sent to DB
    Timestamp = dateconv:get_timestamp(),

    case extract(<<"items">>, Video) of
		{found, [Items]} ->
			
			Id = extract_from_node(<<"id">>, Items),

            Resource_URL = build_youtube_video_link(Id),

			Snippet = extract_from_node(<<"snippet">>, Items),

			PubDate = dateconv:youtube_to_epoch(binary_to_list(extract_from_node(<<"publishedAt">>, Snippet))),		%% convert to EPOCH

			Description = extract_from_node(<<"description">>, Snippet),
			
			Language = extract_from_node(<<"defaultAudioLanguage">>, Snippet),

			Tags = extract_from_node(<<"tags">>, Snippet),

            ChannelId = extract_from_node(<<"channelId">>, Snippet),

            Channel_URL = build_youtube_channel_link(ChannelId),

            ChannelTitle = extract_from_node(<<"channelTitle">>, Snippet),

			Statistics = extract_from_node(<<"statistics">>, Items),

			ViewCount = extract_from_node(<<"viewCount">>, Statistics),

			LikeCount = extract_from_node(<<"likeCount">>, Statistics),

			{Id, PubDate, Description, ViewCount, LikeCount, Tags};

		not_found -> 
			
			Id = null,
            Resource_URL = null,
			PubDate = null,
			Description = null,
			Language = null,
			ViewCount = null,
			LikeCount = null,
			Tags = null,
            ChannelId = null,
            Channel_URL = null,
            ChannelTitle = null,

			{Id, PubDate, Description, ViewCount, LikeCount, Tags}
    end,

    % NOTE: add 'Clean Result'
    A = [{<<"search_term">>, list_to_binary(HashTag)}, {<<"service">>, <<"youtube">>}, {<<"insert_timestamp">>, Timestamp}, {<<"timestamp">>, PubDate}, {<<"content_type">>, <<"video">>}, {<<"service_id">>, Id}, {<<"text">>, Description}, {<<"language">>, Language}, {<<"view_count">>, ViewCount}, {<<"likes">>, LikeCount}, {<<"tags">>, Tags}, {<<"resource_link_high">>, Resource_URL}, {<<"resource_link_low">>, Resource_URL}, {<<"username">>, ChannelTitle}, {<<"profile_link">>, Channel_URL}, {<<"user_id">>, ChannelId}],

    % return clean result
    B = clean_result(A),
    io:format("RETURNED YOUTUBE ITEM ~p~n", [B]),
    B.

%% 
%% @doc Tells if a given Youtube video matches the Language Filter
%%
is_language(Status, LangFilter) ->
	
	case extract_from_node(<<"language">>, Status) of
		null -> 
			io:format("EXTRACTED LANG IS NULL ~n"),
			false;		%% If doc has no Language node, return FALSE
		
		Lang -> 

			Extracted_Lang = lists:sublist(binary_to_list(Lang), 2), 

			io:format("EXTRACTED LANG IS ~p~n", [Extracted_Lang]),
			io:format("LANG FILTER IS ~p~n", [LangFilter]),

			A = binary_to_list(LangFilter) == Extracted_Lang,            
			
			io:format("RETURNING ~p~n", [A]),	

			A
	end.


% ****************************************
% PARSE TWITTER decoded JSON
% ****************************************

% Decode Response Body and parses result
parse_tweet_response_body(HashTag, DecodedBody) ->

    %% Parse the list of Tweets returned by the Twitter API
    case extract(<<"statuses">>, DecodedBody) of
        {found, StatusList} -> 
            parse_tweet_status_list(HashTag, StatusList, []);                                
        not_found -> []                                         %% return empty list if result was empty
    end.

%% 
%% Parse a list of tweets
%% Calls parse_status_details on all statuses included in a given List
%% Sends the document to DB and returns it
parse_tweet_status_list(_HashTag, [], Result) -> Result;                        %% return doc
parse_tweet_status_list(HashTag, [H|T], Result) -> 
    NewResult = Result ++ [parse_tweet_details(HashTag, H)],
    parse_tweet_status_list(HashTag, T, NewResult).

%% Convert single Tweet Object to internal representation form
parse_tweet_details(HashTag, Status) ->

    %% Register the time the document was sent to DB
    Timestamp = dateconv:get_timestamp(),

    %% ======== Parsing Tweet details ==============
    Tweet_ID = case extract(<<"id">>, Status) of
        {found, X1} -> integer_to_binary(X1);
        not_found -> null
    end,

    Date = case extract(<<"created_at">>, Status) of
        {found, X2} -> 
            dateconv:twitter_to_epoch(binary_to_list(X2));
        not_found -> null
    end,

    Text = extract_from_node(<<"text">>, Status),
    
    MetaData = extract_from_node(<<"metadata">>, Status),

    Language = extract_from_node(<<"iso_language_code">>, MetaData),

    Coordinates = extract_from_node(<<"coordinates">>, Status),

	Retweet_Count = extract_from_node(<<"retweet_count">>, Status),

	Favorited = extract_from_node(<<"favorited">>, Status),

    %% Parse 'entities' node in Tweet Object
    case extract(<<"entities">>, Status) of
        {found, Entities} ->
            Tags = case extract(<<"hashtags">>, Entities) of
                {found, []} -> null;
                {found, X8} -> format_tweet_tags(X8);
                not_found -> null
            end,

            % Parse 'Media' entity
            case extract(<<"media">>, Entities) of
                {found, Media} -> 
                    {Media_URL, Media_Type} = format_media_entity(Media);
                not_found -> 
                    Media_URL = null,
                    Media_Type = <<"text">>
            end;

        %% ENTITIES not found
        not_found -> 
            Tags = null,
            Media_URL = null,
            Media_Type = <<"text">>
    end,

    %% ======== Parsing User details ==============

    UserInfo = extract_from_node(<<"user">>, Status),
    UserName = extract_from_node(<<"name">>, UserInfo),
    ScreenName = extract_from_node(<<"screen_name">>, UserInfo),
    User_Profile_Link = build_tweet_profile_link(ScreenName),
    UserID = convert_user_ID(extract_from_node(<<"id">>, UserInfo)),

    A = [{<<"search_term">>, list_to_binary(HashTag)},{<<"service">>, <<"twitter">>}, {<<"service_id">>, Tweet_ID}, {<<"timestamp">>, Date}, {<<"insert_timestamp">>, Timestamp}, {<<"text">>, Text}, {<<"language">>, Language}, {<<"view_count">>, Retweet_Count}, {<<"likes">>, Favorited}, {<<"location">>, Coordinates}, {<<"tags">>, Tags}, {<<"resource_link_high">>, Media_URL}, {<<"resource_link_low">>, Media_URL}, {<<"content_type">>, Media_Type}, {<<"free_text_name">>, UserName}, {<<"username">>, ScreenName}, {<<"profile_link">>, User_Profile_Link}, {<<"user_id">>, UserID}],
    clean_result(A).

% Convert int to binary. Return null if Id is null
convert_user_ID(null) -> null;
convert_user_ID(Id) -> integer_to_binary(Id).

% Format decoded Tweet Tags node 
format_tweet_tags(Tags_List) -> 
    format_tweet_tags(Tags_List, []).

format_tweet_tags([], Result) -> Result;
format_tweet_tags([H|T], Result) -> 
    NewResult = Result ++ [get_single_tag(H)],
    format_tweet_tags(T, NewResult).

get_single_tag([{<<"text">>, A}, _]) -> A;
get_single_tag([{_B, A}, _]) -> A.

% Builds Profile URL for a given user screen_name
build_tweet_profile_link(null) -> null;
build_tweet_profile_link(Screen_Name) -> 
    A = lists:append("https://twitter.com/", binary_to_list(Screen_Name)),
    list_to_binary(A).

% Returns ONLY the first media element information (URL and Type).
format_media_entity([]) -> {null, null};
format_media_entity([H|_T]) -> format_single_media(H).          %% return info related to first media element

format_single_media(Media) ->
    Media_Url = case extract (<<"media_url">>, Media) of
        {found, Url} -> Url;
        not_found -> null
    end,

    Media_Type = case extract (<<"type">>, Media) of
        {found, Type} -> 
            case Type of
                <<"photo">> -> <<"image">>;
                _ -> <<"video">>
            end;
        not_found -> null
    end,

    {Media_Url, Media_Type}.
%%
%% @doc Tells if a given Tweet feed is of one of the types (content-types: text, image, video) included in the list AllowedTypes.
%%
filter_by_content_type(Status, AllowedTypes) -> 

	Extracted_CT = binary_to_list(extract_from_node(<<"content_type">>, Status)),

	lists:member(Extracted_CT, AllowedTypes).