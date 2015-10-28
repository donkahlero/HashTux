-module(tux_twitterminer).

-export([search_hash_tag/1]).

-record(account_keys, {api_key, api_secret,
                       access_token, access_token_secret}).

%% @doc Helper function for 'get_account_keys'
keyfind(Key, L) ->
  {Key, V} = lists:keyfind(Key, 1, L),
  V.

%% @doc Get Account Keys from configuration file
get_account_keys(AccountName) ->
	{ok, Accounts} = application:get_env(tux_miner, tux_miner_accounts),
	{AccountName, Keys} = lists:keyfind(AccountName, 1, Accounts),
	#account_keys{api_key=keyfind(api_key, Keys),
        api_secret=keyfind(api_secret, Keys),
        access_token=keyfind(access_token, Keys),
        access_token_secret=keyfind(access_token_secret, Keys)}.


%% @doc This function WILL search for a given HashTag and print some data
search_hash_tag(HashTag) -> 

	URL = "https://api.twitter.com/1.1/search/tweets.json", 

	Keys = get_account_keys(account1),
  
	%% Check OAUTH for search request
	Consumer = {Keys#account_keys.api_key, Keys#account_keys.api_secret, hmac_sha1},
	AccessToken = Keys#account_keys.access_token,
	AccessTokenSecret = Keys#account_keys.access_token_secret,

  	% Use oauth:sign/6 to generate a list of signed OAuth parameters, 
	SignedParams = oauth:sign("GET", URL, [{q, HashTag}], Consumer, AccessToken, AccessTokenSecret),

    % Send authorized GET request and get result as binary
    Res = ibrowse:send_req(oauth:uri(URL,SignedParams), [], get,[], [{response_format, binary}]),

    io:format("Res is-JSON : ~p~n", [jsx:is_json(Res)]),
    io:format("Res is-TERM : ~p~n", [jsx:is_term(Res)]),

    {ok, Status, _ResponseHeaders, ResponseBody} = Res,

    case Status of
    	"200" -> io:format("Request was fulfilled\n");
    	_Other -> io:format("Got non-200 response\n")
    end,

    DecodedBody = jsx:decode(ResponseBody),

    case extract(<<"statuses">>, DecodedBody) of
    	{found, StatusList} -> 
    		%io:format("Status List is ~p~n", [StatusList]),
    		print_status_list(StatusList);
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
	
    %% extract TWEET information
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

    %% extract USER information 
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





