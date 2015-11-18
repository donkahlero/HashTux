-module(youtube_search).

-export([search/2]).

-define(SEARCH_URL, "https://www.googleapis.com/youtube/v3/search?").		% Endpoint for a Search:list request to Youtube Data API
-define(VIDEOS_URL, "https://www.googleapis.com/youtube/v3/videos?").		% Endpoint for a Videos:list request to Youtube Data API


% @doc Send GET request to Youtube Data API filtering results by the given Keyword
% @params 
%	HashTag: keyword
search(HashTag, [{content_type, Types}, {language, Language}]) ->

	io:format("Youtube: TYPES: ~p~n", [Types]),
	io:format("Youtube: LANGUAGE: ~p~n", [Language]),

	% True if client filtered by content type and requested videos 
	VideoReq = lists:member(<<"video">>, Types),
	io:format("Youtube: VIDEO REQUESTED: ~p~n", [VideoReq]),

	if 
		%% SIMPLE SEARCH or ADVANCED SEARCH that requested Videos
		VideoReq -> 
			
			% A list of the languages available to the client
			LangParams = [<<"en">>, <<"es">>, <<"fr">>, <<"de">>, <<"sv">>, <<"bg">>, <<"it">>, <<"am">>],

			% Check if client requested specific language
			case lists:member(Language, LangParams) of
				%% Query API and Filter Result
				true -> 
					io:format("YOUTUBE: Client requested specific language\n"),
					API_Res = query_youtube_API(HashTag),
					Filtered_Res = [X || X <- API_Res, parser:is_language(X, Language)],
					Res_Length = length(Filtered_Res),
					io:format("YOUTUBE: Filtered Search returned ~p elements ~n", [Res_Length]),
					gen_server:call(db_serv, {add_doc, Filtered_Res}),								% send result to database
					Filtered_Res;																	% return result	
				
				%% Query API (no filter)
				false -> 
					io:format("YOUTUBE: Client requested ALL languages\n"),
					%% **** ADD CALL TO DB Here!!!*****
					Result = query_youtube_API(HashTag),
					Res_Length = length(Result),
					io:format("YOUTUBE: Simple Search returned ~p elements ~n", [Res_Length]),
					gen_server:call(db_serv, {add_doc, Result}),									% send result to database
					Result																			% return result	
			end;

		% Video NOT requested. Return Empty List
		true -> 
			io:format("YOUTUBE: Client DID NOT request VIDEOS. Returning empty list\n"),
			[]																						% return empty list (no data sent to DB)
	end.

% @doc sends a GET request for a given keyword
query_youtube_API(HashTag) ->

	Part = "part=snippet&fields=items(id(videoId))",						%% Partial Request: request only ID 'field' in the Snippet 'part'

	Q = "q=" ++ HashTag,													%% keyword parameter

	UniTime = dateconv:back_one_week(calendar:universal_time()),			%

	FormattedTime = dateconv:datetime_to_rfc_339(UniTime),					% we need to get back 1 week!!!

	After = "publishedAfter=" ++ FormattedTime ++ "&order=date",			% Filter only results from last week and SORT by date

	Type = "type=video&videoCaption=closedCaption",							%% filter only VIDEO 'resource type' that contain capion

	Key = "key=" ++ aux:get_youtube_keys(),											%% API KEY parameter

	Url = ?SEARCH_URL ++ Part ++ "&" ++ Q ++ "&" ++ After ++ "&" ++ Type ++ "&" ++ Key,

	case httpc:request(Url) of 
		{ok, Result} -> 
			{_StatusLine, _Headers, Body} = Result,
            
            DecodedBody = jsx:decode(list_to_binary(Body)),

			case parser:extract(<<"items">>, DecodedBody) of
    			{found, Ids} -> 
    				CleanIds = parser:extract_youtube_ids(Ids),
    				VideoList = [video_search(X) || X <- CleanIds],									% GET a list of decoded Video 'resources' 
    				Results = [parser:parse_youtube_video(Y, HashTag) || Y <- VideoList],			% ***return a list of parsed video items (*** SEND to DB!!!)
    				ResLength = length(Results),
    				io:format("YOUTUBE API Query RETURNED ~p RESULTS~n", [ResLength]),
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

    Part = "part=snippet,statistics,player,status",							%% request details and statistics 'fields'

    Id = "id=" ++ VideoId,													%% id parameter

    Key = "key=" ++ aux:get_youtube_keys(),

	Url = ?VIDEOS_URL ++ Id ++ "&" ++ Key ++ "&" ++ Part,

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