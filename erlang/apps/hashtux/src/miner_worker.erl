-module(miner_worker).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0]).


%%% =======================================================
%%% PUBLIC API
%%% =======================================================

start_link() ->
	io:format("STARTING:miner_worker~n"),
	gen_server:start_link(?MODULE, [], []).


%%% =======================================================
%%% CALLBACK FUNCTIONS
%%% =======================================================


%% ========================================================
init([]) -> 
	{ok, []}.


%% ========================================================
%terminate({'EXIT', _From, _Reason}, _State) ->
% 	io:format("received exit signal in worker~n"),
%	ok;
terminate(_Reason, _State) ->
	ok.


%% ========================================================
code_change(_PrevVersion, _State, _Extra) -> 
	ok.


%% ========================================================
handle_info(_Msg, S) ->
	{noreply, S}.


%% ========================================================
handle_cast({{Pid, _Ref}, Term, Options}, State) ->
	io:format("SEARCH TERM in worker: ~p~n", [Term]),
%	search_hash_tag(Term),
	Pid ! {self(), Term, Options},
	io:format("FINISHED:worker [~p]~n", [self()]),
	{stop, normal, State}.	


%% ========================================================
handle_call({Term, Options}, _From, S) -> 
	{reply, {Term, Options}, S}.


%% @doc This function WILL search for a given HashTag and print some data
search_hash_tag(HashTag) -> 

	URL = "https://api.twitter.com/1.1/search/tweets.json", 

	%Keys = get_account_keys(account1),

    %% TEMP: hardcoded until we restore get_env from config file
    Consumer = {"8zASL19iVZ6IpMa0imBbRDJlo", "u4BL8BIGb6NllmZJwlnO2zpf0H5vzv2wMP4s5aYquZvBFEd0ux", hmac_sha1},
    AccessToken = "3947085676-R5GaAeZAz2ns7wEcfZZ9Fw4Npt62kS4irnkgHcT",
    AccessTokenSecret = "W1raFk3kqVEj38QFjDus2qCfk8IU8ilWgfTqAgk3T5Lv6",

    %%=================================================================%%
    %% UNCOMMENT following lines
	%% Check OAUTH for search request
	% Consumer = {Keys#account_keys.api_key, Keys#account_keys.api_secret, hmac_sha1},
	% AccessToken = Keys#account_keys.access_token,
	% AccessTokenSecret = Keys#account_keys.access_token_secret,
    %%=================================================================%%

  	% Use oauth:sign/6 to generate a list of signed OAuth parameters, 
		SignedParams = oauth:sign("GET", URL, [{q, HashTag}], Consumer, AccessToken, AccessTokenSecret),

		ParsedURI = oauth:uri(URL, SignedParams),
		io:format("PARSED URI: ~p~n", [ParsedURI]),

    % Send authorized GET request and get result as binary
    Res = ibrowse:send_req(ParsedURI, [], get,[], [{response_format, binary}]),
		io:format("RES is : ~p~n", [Res]),
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









