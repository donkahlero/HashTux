-module(twitter_search).

-export([search_hash_tag/2]).

%% Twitter Search API Endpoint
-define(URL, "https://api.twitter.com/1.1/search/tweets.json").

%%
%% @doc Return list of Tweets containing the given HashTag.
%% Tweets are stored in the DB and formatted in 'internal representation' form.
%%
search_hash_tag(Keyword, [{content_type, Types}, {language, Lang}, {history_timestamp, HistoryTimestamp}]) -> 

    HashTag = "#" ++ Keyword,

    % generate Keyword + hisotry_timestamp parameters
    QParam = apis_aux:generate_twitter_q_param(HashTag, HistoryTimestamp),

    % List of available Languages
    LangParams = [<<"en">>, <<"es">>, <<"fr">>, <<"de">>, <<"sv">>, <<"bg">>, <<"it">>, <<"am">>],

    % Set API request parameters
    Options = case lists:member(Lang, LangParams) of
        true -> [{q, QParam}, {lang, binary_to_atom(Lang, latin1)}, {count, 30}, {result_type, recent}];        %% Set 'language' and 'count' params
        false -> [{q, QParam}, {result_type, recent}]
    end,

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
    LangResult = parser:parse_tweet_response_body(Keyword, DecodedBody),

    % Debug Filtered Result size
    LangResLength = length(LangResult),
    io:format("TWITTER SIMPLE SEARCH RETURNED ~p TWEETS~n", [LangResLength]),

    %% FILTER LangResult by content_type
    FilteredRes = [H || H <- LangResult, parser:filter_by_content_type(H, TypeFilter)],

    % Debug Filtered Result size
    ResLength = length(FilteredRes),
    io:format("TWITTER ADVANCED SEARCH RETURNED ~p TWEETS~n", [ResLength]),

    % Return Filtered Result
    [{filtered, FilteredRes}, {unfiltered, LangResult}].         %% Return Result

% Print request status information
print_response_info(Status) ->
    case Status of
        "200" -> io:format("Twitter Request was fulfilled\n");
        _Other -> io:format("Twitter Request got non-200 response\n")
    end.