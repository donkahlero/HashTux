-module(twitter_search).

-export([search_hash_tag/2]).

%% Twitter Search API Endpoint
-define(URL, "https://api.twitter.com/1.1/search/tweets.json").

% ADVANCED SEARCH: Filtered by Language and Type [text, image, video].
search_hash_tag(HashTag, [{content_type, Types}, {language, Lang}]) -> 
    
    io:format("TWITTER ADVANCED SEARCH CALLED~n"),

    Count = 30,         %% MAX number of results returned by 'GET' request.

    % List of available Languages
    LangParams = [<<"en">>, <<"es">>, <<"fr">>, <<"de">>, <<"sv">>, <<"bg">>, <<"it">>, <<"am">>],

    % Set API request parameters
    Options = case lists:member(Lang, LangParams) of
        true -> [{q, HashTag}, {lang, binary_to_atom(Lang, latin1)}, {count, Count}, {result_type, recent}];
        false -> [{q, HashTag}, {result_type, recent}]                  %% do not set Language parameter in API request
    end,

    io:format("Twitter: TYPES: ~p~n", [Types]),
    % Set content_type filter
    TypeFilter = [binary_to_list(Z) || Z <- Types],  

    % Get Authorization Credentials
    {AccessToken, AccessTokenSecret, ConsumerKey, ConsumerKeySecret} = aux:get_twitter_keys(),
    Consumer = {ConsumerKey, ConsumerKeySecret, hmac_sha1},  

    % Use oauth:sign/6 to generate a list of signed OAuth parameters, 
    SignedParams = oauth:sign("GET", ?URL, Options, Consumer, AccessToken, AccessTokenSecret),

    % Send authorized GET request and get result as binary
    Res = ibrowse:send_req(oauth:uri(?URL,SignedParams), [], get,[], [{response_format, binary}]),

    {ok, Status, _ResponseHeaders, ResponseBody} = Res,

    print_response_info(Status),

    %% Decode response body 
    DecodedBody = jsx:decode(ResponseBody),
    %% LangResult is a list of Internal JSX objects mined for a specified language (all languages if not specified)
    LangResult = parser:parse_tweet_response_body(HashTag, DecodedBody),

    %% FILTER LangResult by content_type
    FilteredRes = [H || H <- LangResult, parser:is_content_type(H, TypeFilter)],

    % Debug Filtered Result size
    ResLength = length(FilteredRes),
    io:format("TWITTER ADVANCED SEARCH RETURNED ~p TWEETS~n", [ResLength]),

    gen_server:call(db_serv, {add_doc, FilteredRes}),            %% Sends the document to DB

    % Return Filtered Result
    FilteredRes.                                                 %% Return Result

% Print request status information
print_response_info(Status) ->
    case Status of
        "200" -> io:format("Request was fulfilled\n");
        _Other -> io:format("Got non-200 response\n")
    end.
