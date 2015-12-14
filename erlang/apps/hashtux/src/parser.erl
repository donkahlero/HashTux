%% @author Marco Trifance <marco.trifance@gmail.com>
%% @doc A parser for decoded JSON Items. Handle information conversion 
%%      and formatting from decoded Twitter and Youtube FEEDS into internal representation format

-module(parser).

-export([extract/2, atom_to_binarystring/1]).
-export([extract_youtube_ids/1, parse_youtube_video/2, clean_result/1, is_language/2]).						
-export([filter_by_content_type/2, parse_twitter_response_body/2]).											

%% @doc Search the value of a given key 'K' from a given list 'L' decoded JSON message.
extract(K, L) ->
  case lists:keyfind(K, 1, L) of
    {_, M} -> {found, M};
    false  -> not_found
  end.


%% @doc Extract the value of a field from a parent node. 
%%      Return value if field was found, null if it was not found.
extract_from_node(_Field, null) -> null;
extract_from_node(Field, Node) -> 
	case extract(Field, Node) of
		{found, X} -> X; 
		not_found -> null
    end.

%% @doc Remove all fields with null value from parsed feed. 
clean_result(L) -> [X || X <- L, has_null_value(X) == false].

%% @doc Helper function for clean_result/1.
%%      Return True if a field in a converted feed has null value.
has_null_value({_, null}) -> true;
has_null_value({_, _}) -> false.

%% @doc Convert an atom to binary_string
atom_to_binarystring(Atom) ->
    list_to_binary(atom_to_list(Atom)).

%% @doc Extract the Id from a list of decoded Youtube feeds returned by a Keyword search.
extract_youtube_ids(List) ->
	extract_ids(List, []).

%% @doc Helper function for extract_youtube_ids/1.
extract_ids([], Result) -> Result;
extract_ids([H|T], Result) ->
	NewResult = Result ++ [extract_id(H)],
	extract_ids(T, NewResult).

%% @doc Helper function for extract_ids/2.
%%      Return the videoId in binary-string form from a Youtube "id" node.
extract_id([{<<"id">>, [{<<"videoId">>, VideoId}]}]) -> binary_to_list(VideoId);
extract_id(_) -> parser_error.

%% @doc Create a Youtube Channel URL for a given channelId.
build_youtube_channel_link(null) -> null;
build_youtube_channel_link(ChannelId) -> 
    A = lists:append("https://youtube.com/channel/", binary_to_list(ChannelId)),
    list_to_binary(A).

%% @doc Create a Youtube Video URL for a given channelId.
build_youtube_video_link(null) -> null;
build_youtube_video_link(VideoId) -> 
    A = lists:append("https://www.youtube.com/watch?v=", binary_to_list(VideoId)),
    list_to_binary(A).

%% @doc Create a Youtube 'Embedded Video' URL for a given videoId.
build_youtube_embedded_link(null) -> null;
build_youtube_embedded_link(VideoId) -> 
    A = lists:append("https://www.youtube.com/embed/", binary_to_list(VideoId)),
    list_to_binary(A).

%% @doc Convert a decoded Youtube Video resource to internal representation
parse_youtube_video(Video, HashTag) -> 
	
	%% Register the time the document was sent to DB
    Timestamp = dateconv:get_timestamp(),

    case extract(<<"items">>, Video) of
		{found, [Items]} ->
			
			Id = extract_from_node(<<"id">>, Items),

            Resource_URL = build_youtube_video_link(Id),

            Embed_URL = build_youtube_embedded_link(Id),

			Snippet = extract_from_node(<<"snippet">>, Items),

			Date = extract_from_node(<<"publishedAt">>, Snippet),

            PubDate = apis_aux:youtube_to_epoch(binary_to_list(extract_from_node(<<"publishedAt">>, Snippet))),		%% convert to EPOCH

			Description = extract_from_node(<<"description">>, Snippet),
			
			Language = extract_from_node(<<"defaultAudioLanguage">>, Snippet),

			Tags = extract_from_node(<<"tags">>, Snippet),

            ChannelId = extract_from_node(<<"channelId">>, Snippet),

            Channel_URL = build_youtube_channel_link(ChannelId),

            ChannelTitle = extract_from_node(<<"channelTitle">>, Snippet),

			Statistics = extract_from_node(<<"statistics">>, Items),

			ViewCount = extract_from_node(<<"viewCount">>, Statistics),

			LikeCount = extract_from_node(<<"likeCount">>, Statistics);

		not_found -> 
			
			Id = null,
            Resource_URL = null,
            Embed_URL = null,
			Date = null,
            PubDate = null,
			Description = null,
			Language = null,
			ViewCount = null,
			LikeCount = null,
			Tags = null,
            ChannelId = null,
            Channel_URL = null,
            ChannelTitle = null
    end,

    A = [{<<"search_term">>, list_to_binary(HashTag)}, {<<"service">>, <<"youtube">>}, {<<"insert_timestamp">>, Timestamp}, {<<"timestamp">>, PubDate}, {<<"date_string">>, Date}, {<<"content_type">>, <<"video">>}, {<<"service_id">>, Id}, {<<"text">>, Description}, {<<"language">>, Language}, {<<"view_count">>, ViewCount}, {<<"likes">>, LikeCount}, {<<"tags">>, Tags}, {<<"resource_link_high">>, Embed_URL}, {<<"resource_link_low">>, Resource_URL}, {<<"username">>, ChannelTitle}, {<<"profile_link">>, Channel_URL}, {<<"user_id">>, ChannelId}],

    %% Remove empty fields from parsed Youtube video item
    clean_result(A).

%% @doc Compare the value in the language field of an internal Youtube video resource with 
%%      the given LanguageFilter.
%%      Return True if a given Youtube video matches the Language Filter, false otherwise.
is_language(Status, LangFilter) ->
	
	case extract_from_node(<<"language">>, Status) of
		null -> 
			io:format("EXTRACTED LANG IS NULL ~n"),
			false;		%% If doc has no Language node, return FALSE
		
		Lang -> 

			Extracted_Lang = lists:sublist(binary_to_list(Lang), 2), 

			binary_to_list(LangFilter) == Extracted_Lang
	end.

%% @doc Decode Response Body and extract list of twitter statuses
%%      in internal representation form.
parse_twitter_response_body(HashTag, DecodedBody) ->

    %% Parse the list of Tweets returned by the Twitter API
    case extract(<<"statuses">>, DecodedBody) of
        %% Parse status list if query to Twitter API returned a list of Tweets
        {found, StatusList} -> 
            parse_tweet_status_list(HashTag, StatusList, []);                                
        %% Return Empty list if query to Twitter API returned empty statuses list
        not_found -> []                                         
    end.

%% @doc Helper function for parse_twitter_response_body/2
%%      Parse a list of JSX decoded tweets
parse_tweet_status_list(_HashTag, [], Result) -> Result;                        %% return doc
parse_tweet_status_list(HashTag, [H|T], Result) -> 
    NewResult = Result ++ [parse_tweet_details(HashTag, H)],
    parse_tweet_status_list(HashTag, T, NewResult).

%% @doc Helper function for parse_twitter_response_body/2
%%      Convert a single decoded Tweet Object to internal representation form
parse_tweet_details(HashTag, Status) ->

    %% Register the time the document was sent to DB
    Timestamp = dateconv:get_timestamp(),

    %% Parsing Tweet details 
    Tweet_ID = case extract(<<"id">>, Status) of
        {found, X1} -> integer_to_binary(X1);
        not_found -> null
    end,

    StringDate = extract_from_node(<<"created_at">>, Status),

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

    %% Parse User details 
    UserInfo = extract_from_node(<<"user">>, Status),
    UserName = extract_from_node(<<"name">>, UserInfo),
    ScreenName = extract_from_node(<<"screen_name">>, UserInfo),
    User_Profile_Link = build_tweet_profile_link(ScreenName),
    User_Profile_Image_Url = extract_from_node(<<"profile_image_url_https">>, UserInfo),

    UserID = convert_user_ID(extract_from_node(<<"id">>, UserInfo)),

    A = [{<<"search_term">>, list_to_binary(HashTag)},{<<"service">>, <<"twitter">>}, {<<"service_id">>, Tweet_ID}, {<<"timestamp">>, Date}, {<<"date_string">>, StringDate}, {<<"insert_timestamp">>, Timestamp}, {<<"text">>, Text}, {<<"language">>, Language}, {<<"view_count">>, Retweet_Count}, {<<"likes">>, Favorited}, {<<"location">>, Coordinates}, {<<"tags">>, Tags}, {<<"resource_link_high">>, Media_URL}, {<<"resource_link_low">>, Media_URL}, {<<"content_type">>, Media_Type}, {<<"free_text_name">>, UserName}, {<<"username">>, ScreenName}, {<<"profile_link">>, User_Profile_Link}, {<<"profile_image_url">>, User_Profile_Image_Url}, {<<"user_id">>, UserID}],
    %% Remove empty fields from parsed Tweet item
    clean_result(A).

%% @doc Convert Twitter user_ID int to binary. Return null if Id is null
convert_user_ID(null) -> null;
convert_user_ID(Id) -> integer_to_binary(Id).

%% @doc Format decoded Tweet Tags node 
format_tweet_tags(Tags_List) -> 
    format_tweet_tags(Tags_List, []).

%% @doc Helper function for format_tweet_tags/1
format_tweet_tags([], Result) -> Result;
format_tweet_tags([H|T], Result) -> 
    NewResult = Result ++ [get_single_tag(H)],
    format_tweet_tags(T, NewResult).

%% @doc Helper function for format_tweet_tags/2
get_single_tag([{<<"text">>, A}, _]) -> A;
get_single_tag([{_B, A}, _]) -> A.

%% @doc Create a Twitter profile URL for a given user screen_name
build_tweet_profile_link(null) -> null;
build_tweet_profile_link(Screen_Name) -> 
    A = lists:append("https://twitter.com/", binary_to_list(Screen_Name)),
    list_to_binary(A).

%% @doc Returns ONLY the FIRST media element information (URL and Type).
format_media_entity([]) -> {null, null};
format_media_entity([H|_T]) -> format_single_media(H).          

%% @doc Extract "media_url" and "type" fields information 
%%      from a Media node in a Twitter status resource.
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

%% @doc Return True if a given Twitter Status is of one of the types 
%%      (content-types: text, image, video) included in the list AllowedTypes.
filter_by_content_type(Status, AllowedTypes) -> 
	Extracted_CT = binary_to_list(extract_from_node(<<"content_type">>, Status)),
	lists:member(Extracted_CT, AllowedTypes).