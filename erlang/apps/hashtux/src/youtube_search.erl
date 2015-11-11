-module(youtube_search).

-export([search/2]).

%% To be included in CONFIG FILE!
-define(SERVER_KEY, "AIzaSyAT27JOYa8DAQKFK_2vPfxWagLxMXqbXFY").

% @doc Send GET request to Youtube Data API filtering results by the given Keyword
% @params 
%	HashTag: keyword
search(HashTag, _Options) ->

	Endpoint = "https://www.googleapis.com/youtube/v3/search?",             %% endpoint for ITEM-LIST

	Part = "part=snippet&fields=items(id(videoId))",						%% Partial Request: request only ID 'field' in the Snippet 'part'

	Q = "q=" ++ HashTag,													%% keyword parameter

	UniTime = dateconv:back_one_week(calendar:universal_time()),			%

	FormattedTime = dateconv:datetime_to_rfc_339(UniTime),					% we need to get back 1 week!!!

	After = "publishedAfter=" ++ FormattedTime,								% Filter only results from one week

	Type = "type=video&videoCaption=closedCaption",							%% filter only VIDEO 'resource type' that contain capion

	Key = "key=" ++ ?SERVER_KEY,											%% API KEY parameter

	Url = Endpoint ++ Part ++ "&" ++ Q ++ "&" ++ After ++ "&" ++ Type ++ "&" ++ Key,

	case httpc:request(Url) of 
		{ok, Result} -> 
			{_StatusLine, _Headers, Body} = Result,
            
            DecodedBody = jsx:decode(list_to_binary(Body)),

			case parser:extract(<<"items">>, DecodedBody) of
    			{found, Ids} -> 
    				CleanIds = parser:extract_youtube_ids(Ids),
    				VideoList = [video_search(X) || X <- CleanIds],									% GET a list of decoded Video 'resources' 
    				Results = [parser:parse_youtube_video(Y, HashTag) || Y <- VideoList],			% ***return a list of parsed video items (*** SEND to DB!!!)
    				gen_server:call(db_serv, {add_doc, Results}),									% send result to database
    				Results;																		% return Youtube results
    			not_found -> 
    				io:format("ITEMS List NOT FOUND\n"),
    				[]																				% return an empty list if items is not found
    		end;
		
		{error, Reason} ->
			io:format("HTTP request to YOUTUBE API failed for reason ~p~n", [Reason]);
		Other -> io:format("OTHER is ~p~n", [Other])
	end.

% @doc sends a GET request for a specific videoId 
video_search(VideoId) ->

	Endpoint = "https://www.googleapis.com/youtube/v3/videos?",             %% endpoint for DETAILS

    Part = "part=snippet,statistics,player,status",							%% request details and statistics 'fields'

    Id = "id=" ++ VideoId,													%% id parameter

    Key = "key=" ++ ?SERVER_KEY,

	Url = Endpoint ++ Id ++ "&" ++ Key ++ "&" ++ Part,

	case httpc:request(Url) of 
		{ok, Result} -> 
			{_StatusLine2, _Headers2, Body} = Result,
            jsx:decode(list_to_binary(Body));
		{error, Reason2} ->
			io:format("CONTENT DETAILS request to YOUTUBE API failed for reason ~p~n", [Reason2]),
			null;
		Other2 -> 
			io:format("CONTENT DETAILS request to YOUTUBE API failed. Received ~p~n", [Other2]),
			null
	end.