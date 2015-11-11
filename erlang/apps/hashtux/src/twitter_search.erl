-module(twitter_search).

-export([search_hash_tag/2]).

%% Define macros for Application Authentication
-define(URL, "https://api.twitter.com/1.1/search/tweets.json").
-define(CONSUMER, {"8zASL19iVZ6IpMa0imBbRDJlo", "u4BL8BIGb6NllmZJwlnO2zpf0H5vzv2wMP4s5aYquZvBFEd0ux", hmac_sha1}).
-define(ACCES_TOKEN, "3947085676-R5GaAeZAz2ns7wEcfZZ9Fw4Npt62kS4irnkgHcT").
-define(ACCES_TOKEN_SECRET, "W1raFk3kqVEj38QFjDus2qCfk8IU8ilWgfTqAgk3T5Lv6").


search_hash_tag(HashTag, [Qty, Lang]) -> 
    
    Count = if 
        (Qty < 0) -> 15;
        (Qty > 100) -> 100;
        true -> Qty
    end,

    Options = case Lang of
        en -> [{q, HashTag}, {lang, Lang}, {count, Count}, {response_format, binary}];
        it -> [{q, HashTag}, {lang, Lang}, {count, Count}, {response_format, binary}];
        fr -> [{q, HashTag}, {lang, Lang}, {count, Count}, {response_format, binary}];
        es -> [{q, HashTag}, {lang, Lang}, {count, Count}, {response_format, binary}];
        bg -> [{q, HashTag}, {lang, Lang}, {count, Count}, {response_format, binary}];
        de -> [{q, HashTag}, {lang, Lang}, {count, Count}, {response_format, binary}];
        sv -> [{q, HashTag}, {lang, Lang}, {count, Count}, {response_format, binary}];
        am -> [{q, HashTag}, {lang, Lang}, {count, Count}, {response_format, binary}];
        _ -> [{q, HashTag}, {count, Count}, {response_format, binary}]                    %% Catch all- do not filter for any language
    end, 

    % Use oauth:sign/6 to generate a list of signed OAuth parameters, 
    SignedParams = oauth:sign("GET", ?URL, Options, ?CONSUMER, ?ACCES_TOKEN, ?ACCES_TOKEN_SECRET),

    % Send authorized GET request and get result as binary
    Res = ibrowse:send_req(oauth:uri(?URL,SignedParams), [], get,[], Options),

    {ok, Status, _ResponseHeaders, ResponseBody} = Res,

    print_response_info(Status),

    parse_response_body(HashTag, ResponseBody);

%% DEFAULT SEARCH: match all options
search_hash_tag(HashTag, _Options) -> 
  	% Use oauth:sign/6 to generate a list of signed OAuth parameters, 
	SignedParams = oauth:sign("GET", ?URL, [{q, HashTag}], ?CONSUMER, ?ACCES_TOKEN, ?ACCES_TOKEN_SECRET),

    % Send authorized GET request and get result as binary
    Res = ibrowse:send_req(oauth:uri(?URL,SignedParams), [], get,[], [{response_format, binary}]),

    {ok, Status, _ResponseHeaders, ResponseBody} = Res,

    print_response_info(Status),

    parse_response_body(HashTag, ResponseBody).

% Print request status information
print_response_info(Status) ->
    case Status of
        "200" -> io:format("Request was fulfilled\n");
        _Other -> io:format("Got non-200 response\n")
    end.

% Decode Response Body and parses result
parse_response_body(HashTag, ResponseBody) ->
    DecodedBody = jsx:decode(ResponseBody),

    case extract(<<"search_metadata">>, DecodedBody) of
        {found, SearchMetadata} -> 
            % io:format("Search Metadata is ~p~n", [SearchMetadata]),
            
            case extract(<<"count">>, SearchMetadata) of
                {found, SearchCount} -> io:format("Search Count is ~p~n", [SearchCount]);
                not_found -> io:format("Search Count NOT FOUND\n")
            end;

        not_found -> io:format("Search Metadata NOT FOUND\n")
    end,

    case extract(<<"statuses">>, DecodedBody) of
        {found, StatusList} -> 
            parse_status_list(HashTag, StatusList, []);                                
        not_found -> []
    end.

    
% the value of a key from decoded JSON message.
extract(K, L) ->
  case lists:keyfind(K, 1, L) of
    {_, M} -> {found, M};
    false  -> not_found
  end.

% Builds Profile URL for a given user screen_name
build_profile_link(Screen_Name) -> 
    A = lists:append("https://twitter.com/", binary_to_list(Screen_Name)),
    list_to_binary(A).

% Format Tags from JSON object format
format_tags(Tags_List) -> 
    format_tags(Tags_List, []).

format_tags([], Result) -> Result;
format_tags([H|T], Result) -> 
    NewResult = Result ++ [get_single_tag(H)],
    format_tags(T, NewResult).

get_single_tag([{<<"text">>, A}, _]) -> A;
get_single_tag([{_B, A}, _]) -> A.

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

% Remove empty fields from Tweet Result (i.e. removes fields that returned null)
clean_result(L) -> [X || X <- L, has_null_value(X) == false].

has_null_value({_, null}) -> true;
has_null_value({_, _}) -> false.

%% PARSING!!!!
%% Calls parse_status_details on all statuses included in a given List
%% Sends the document to DB and returns it
parse_status_list(_HashTag, [], Result) -> 
    gen_server:call(db_serv, {add_doc, Result}),            %% Sends the document to DB
    Result;                                                 %% return doc
parse_status_list(HashTag, [H|T], Result) -> 
    NewResult = Result ++ [parse_status_details(HashTag, H)],
    parse_status_list(HashTag, T, NewResult).

%% Convert single Tweet Object to internal representation form
parse_status_details(HashTag, Status) ->

    %% Register the time the document was sent to DB
    Timestamp = dateconv:get_timestamp(),

    %% ======== Parsing Tweet details ==============
    Tweet_ID = case extract(<<"id">>, Status) of
        {found, X1} -> integer_to_binary(X1);
        not_found -> null
    end,

    Date = case extract(<<"created_at">>, Status) of
        {found, X2} -> dateconv:twitter_to_epoch(binary_to_list(X2));
        not_found -> null
    end,

    Text = case extract(<<"text">>, Status) of
        {found, X3} -> X3;
        not_found -> null
    end,

    case extract(<<"metadata">>, Status) of
        {found, MetaData} -> 
            
            Language = case extract(<<"iso_language_code">>, MetaData) of
                {found, X4} -> X4;
                not_found -> null
            end;

        not_found -> 
            Language = null
    end,

    Coordinates = case extract(<<"coordinates">>, Status) of
        {found, X5} -> X5;
        not_found -> null
    end,    

    Retweet_Count = case extract(<<"retweet_count">>, Status) of
        {found, X6} -> integer_to_binary(X6);
        not_found -> null
    end,

    Favorited = case extract(<<"favorited">>, Status) of
        {found, false} -> null;
        {found, X7} -> X7;
        not_found -> null
    end,

    %% Parse 'entities' node in Tweet Object
    case extract(<<"entities">>, Status) of
        {found, Entities} ->
            Tags = case extract(<<"hashtags">>, Entities) of
                {found, []} -> null;
                {found, X8} -> format_tags(X8);
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

    case extract(<<"user">>, Status) of
        {found, UserInfo} -> 
            
            UserName = case extract(<<"name">>, UserInfo) of
                {found, Y1} -> Y1;
                not_found -> null
            end,

            ScreenName = case extract(<<"screen_name">>, UserInfo) of
                {found, Y4} -> Y4;
                not_found -> null
            end, 

            User_Profile_Link = case extract(<<"screen_name">>, UserInfo) of
                {found, Y2} -> build_profile_link(Y2);
                not_found -> null
            end,

            UserID = case extract(<<"id">>, UserInfo) of
                {found, Y3} -> integer_to_binary(Y3);
                not_found -> null
            end;

        not_found -> 
            UserName = null,
            ScreenName = null,
            User_Profile_Link = null,
            UserID = null
    end,


    A = [{<<"search_term">>, list_to_binary(HashTag)},{<<"service">>, <<"twitter">>}, {<<"service_id">>, Tweet_ID}, {<<"timestamp">>, Date}, {<<"insert_timestamp">>, Timestamp}, {<<"text">>, Text}, {<<"language">>, Language}, {<<"view_count">>, Retweet_Count}, {<<"likes">>, Favorited}, {<<"location">>, Coordinates}, {<<"tags">>, Tags}, {<<"resource_link_high">>, Media_URL}, {<<"resource_link_low">>, Media_URL}, {<<"content_type">>, Media_Type}, {<<"free_text_name">>, UserName}, {<<"username">>, ScreenName}, {<<"profile_link">>, User_Profile_Link}, {<<"user_id">>, UserID}],
    clean_result(A).
