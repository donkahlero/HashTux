-module(twitter_search).

-export([search_hash_tag/2]).

%% Define macros for Application Authentication
-define(URL, "https://api.twitter.com/1.1/search/tweets.json").
-define(CONSUMER, {"8zASL19iVZ6IpMa0imBbRDJlo", "u4BL8BIGb6NllmZJwlnO2zpf0H5vzv2wMP4s5aYquZvBFEd0ux", hmac_sha1}).
-define(ACCES_TOKEN, "3947085676-R5GaAeZAz2ns7wEcfZZ9Fw4Npt62kS4irnkgHcT").
-define(ACCES_TOKEN_SECRET, "W1raFk3kqVEj38QFjDus2qCfk8IU8ilWgfTqAgk3T5Lv6").


%% @doc This function WILL search for a given HashTag and print some data
search_hash_tag(HashTag, _Options) -> 
    %%=================================================================%%
    %% UNCOMMENT following lines
	%% Check OAUTH for search request
	% Consumer = {Keys#account_keys.api_key, Keys#account_keys.api_secret, hmac_sha1},
	% AccessToken = Keys#account_keys.access_token,
	% AccessTokenSecret = Keys#account_keys.access_token_secret,
    %%=================================================================%%

  	% Use oauth:sign/6 to generate a list of signed OAuth parameters, 
	SignedParams = oauth:sign("GET", ?URL, [{q, HashTag}], ?CONSUMER, ?ACCES_TOKEN, ?ACCES_TOKEN_SECRET),

    % Send authorized GET request and get result as binary
    Res = ibrowse:send_req(oauth:uri(?URL,SignedParams), [], get,[], [{response_format, binary}]),

    {ok, Status, _ResponseHeaders, ResponseBody} = Res,

    case Status of
    	"200" -> io:format("Request was fulfilled\n");
    	_Other -> io:format("Got non-200 response\n")
    end,

    DecodedBody = jsx:decode(ResponseBody),

    case extract(<<"statuses">>, DecodedBody) of
    	{found, StatusList} -> 
    		%io:format("Status List is ~p~n", [StatusList]),
    		%print_status_list(StatusList);                        
            parse_status_list(HashTag, StatusList);                                % uncomment this!!!
    	not_found -> io:format("Status List NOT FOUND\n")
    end,

    case extract(<<"search_metadata">>, DecodedBody) of
    	{found, SearchMetadata} -> 
    		% io:format("Search Metadata is ~p~n", [SearchMetadata]),
			
			case extract(<<"count">>, SearchMetadata) of
    			{found, SearchCount} -> io:format("Search Count is ~p~n", [SearchCount]);
    			not_found -> io:format("Search Count NOT FOUND\n")
    		end;

    	not_found -> io:format("Search Metadata NOT FOUND\n")
    end.

    
% the value of a key from decoded JSON message.
extract(K, L) ->
  case lists:keyfind(K, 1, L) of
    {_, M} -> {found, M};
    false  -> not_found
  end.

%% Calls print_status_details on all statuses included in a given List
print_status_list([]) -> ok;
print_status_list([H|T]) -> 
	print_status_details(H),
	print_status_list(T).

%% Prints details for a specific status
print_status_details(Status) ->
	
    %% ===== extract TWEET information ================
    case extract(<<"id">>, Status) of
        {found, Tweet_ID} -> io:format("Tweet_ID : ~p~n", [Tweet_ID]);
        not_found -> io:format("Tweet_ID NOT FOUND\n")
    end,

    case extract(<<"created_at">>, Status) of
		{found, CreationDate} -> io:format("Creation Date : ~p~n", [binary:bin_to_list(CreationDate)]);
		not_found -> io:format("Creation Date NOT FOUND\n")
	end,

	case extract(<<"text">>, Status) of
		{found, StatusText} -> 
			io:format("Text : ~p~n", [binary:bin_to_list(StatusText)]);
		not_found -> io:format("Status Text NOT FOUND\n")
	end,

    case extract(<<"retweet_count">>, Status) of
        {found, Retweet_Count} -> io:format("Retweet count : ~p~n", [Retweet_Count]);
        not_found -> io:format("Retweet count NOT FOUND\n")
    end,

    case extract(<<"entities">>, Status) of
        {found, Entities} -> 
            io:format("ENTITIES Content:\n"),

            case extract(<<"urls">>, Entities) of
                {found, Urls} -> 
                    io:format("Urls LIST : ~p~n", [Urls]);
                not_found -> io:format("Urls LIST NOT FOUND\n")
            end,

            case extract(<<"hashtags">>, Entities) of
                {found, HashTags} -> 
                    io:format("HashTags List : ~p~n", [HashTags]);
                not_found -> io:format("HashTags List LIST NOT FOUND\n")
            end,

            case extract(<<"user_mentions">>, Entities) of
                {found, UserMentions} -> 
                    io:format("User Mentions List : ~p~n", [UserMentions]);
                not_found -> io:format("User Mentions List LIST NOT FOUND\n")
            end;

        not_found -> io:format("Entities NOT FOUND\n")
    end,    

    %% ===== extract USER information ================
    case extract(<<"user">>, Status) of
        {found, UserInfo} -> 
            
            case extract(<<"name">>, UserInfo) of
                {found, UserName} -> io:format("Posted by : ~p~n", [binary:bin_to_list(UserName)]);
                not_found -> io:format("User Name NOT FOUND\n")
            end;

        not_found -> io:format("User Info NOT FOUND\n")
    end,

    %% Print Empty line
    io:format("\n").

%% PARSING!!!!
%% Calls print_status_details on all statuses included in a given List
parse_status_list(_HashTag, []) -> ok;
parse_status_list(HashTag, [H|T]) -> 
    parse_status_details(HashTag, H),
    parse_status_list(HashTag, T).

% Gets expanded URL from URL list included in Entities in a Status Object
get_expanded_urls([]) -> null;
get_expanded_urls(List) ->
    get_expanded_urls(List, []).

get_expanded_urls([], Result) -> Result;
get_expanded_urls([H|T], Result) -> 
    New_Result = Result ++ [get_expanded_url(H)],
    get_expanded_urls(T, New_Result).

get_expanded_url([]) -> null;
get_expanded_url(List) -> 
    case extract(<<"expanded_url">>, List) of
        {found, X} -> X;
        not_found -> null
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

% Remove empty fields from Tweet Result (i.e. removes fields that returned null)
clean_result(L) -> [X || X <- L, has_null_value(X) == false].

has_null_value({_, null}) -> true;
has_null_value({_, _}) -> false.

parse_status_details(HashTag, Status) ->

    %% ======== Parsing Tweet details ==============
    Tweet_ID = case extract(<<"id">>, Status) of
        {found, X1} -> integer_to_binary(X1);
        not_found -> null
    end,

    Date = case extract(<<"created_at">>, Status) of
        {found, X2} -> X2;
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

    case extract(<<"entities">>, Status) of
        {found, Entities} ->
            Tags = case extract(<<"hashtags">>, Entities) of
                {found, []} -> null;
                {found, X8} -> format_tags(X8);
                not_found -> null
            end,

            Res_Links = case extract(<<"urls">>, Entities) of
                {found, X9} -> get_expanded_urls(X9);
                not_found -> null
            end;

        %% ENTITIES not found
        not_found -> 
            Tags = null,
            Res_Links = null
    end,


    %% ======== Parsing User details ==============

    case extract(<<"user">>, Status) of
        {found, UserInfo} -> 
            
            UserName = case extract(<<"name">>, UserInfo) of
                {found, Y1} -> Y1;
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
            User_Profile_Link = null,
            UserID = null
    end,


    A = [{<<"search_term">>, list_to_binary(HashTag)},{<<"social_media">>, <<"Twitter">>}, {<<"service_id">>, Tweet_ID}, {<<"date">>, Date}, {<<"text">>, Text}, {<<"language">>, Language}, {<<"view_count">>, Retweet_Count}, {<<"likes">>, Favorited}, {<<"location">>, Coordinates}, {<<"tags">>, Tags}, {<<"resource_link">>, Res_Links}, {<<"username">>, UserName}, {<<"profile_link">>, User_Profile_Link}, {<<"user_id">>, UserID}],
    io:format("RAW Result is ~p~n", [A]),
    B = clean_result(A),
    io:format("Returning CLEAN Result: ~p~n", [B]),
    B.
