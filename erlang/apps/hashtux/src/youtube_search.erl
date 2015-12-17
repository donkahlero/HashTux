%% @author Marco Trifance <marco.trifance@gmail.com>
%% @doc Handle Youtube Data API search requests according to HashTux filters and parameters.

-module(youtube_search).

-export([search/2]).

%% Endpoint for a Search:list request to Youtube Data API
-define(SEARCH_URL, "https://www.googleapis.com/youtube/v3/search?").		
%% Endpoint for a Videos:list request to Youtube Data API
-define(VIDEOS_URL, "https://www.googleapis.com/youtube/v3/videos?").		

%% @doc     Sends a GET request to the 'Youtube Data API'.
%% @return  A list of two tuples in the following format: [{filtered, FilteredRes}, {unfiltered, LangResult}].
%%          UnfilteredRes: a list of youtube videos in internal representation form matching the specified values
%%          for history timestamp param;
%%          FilteredRes: a list of all videos from UnfilteredRes that match the language requestedby the client. 
%% @params  Types: a list of types [text, image, video] of feeds requested by the client.
%% @params  Lang: the langauge of the feeds the query will request
%% @HistoryTimestamp: the publication date of the feeds the query will request (epoch timestamp)
search(HashTag, [{content_type, Types}, {language, Language}, {history_timestamp, HistoryTimestamp}]) ->

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
				%% Query API and Filter Result by Language
				true -> 
					io:format("YOUTUBE: Client requested specific language\n"),
					API_Res = query_youtube_API(HashTag, 30, HistoryTimestamp),
					Filtered_Res = [X || X <- API_Res, parser:is_language(X, Language)],
					Res_Length = length(Filtered_Res),
					io:format("YOUTUBE: Filtered Search returned ~p elements ~n", [Res_Length]),
					[{filtered, Filtered_Res}, {unfiltered, API_Res}];											% return result	
				
				%% Query API (no language-filter)
				false -> 
					io:format("YOUTUBE: Client requested ALL languages\n"),
					Result = query_youtube_API(HashTag, 10, HistoryTimestamp),
					Res_Length = length(Result),
					io:format("YOUTUBE: Simple Search returned ~p elements ~n", [Res_Length]),
					[{filtered, Result}, {unfiltered, Result}]													% return result	
			end;

		% Video NOT requested. Return Empty List
		true -> 
			io:format("YOUTUBE: Client DID NOT request VIDEOS. Returning empty list\n"),
			[{filtered, []}, {unfiltered, []}]																						% return empty list (no data sent to DB)
	end.

%% 	@doc Helper function for search/2 
%%	Sends a GET request using the Search:list method for a given keyword

query_youtube_API(HashTag, Count, HistoryTimestamp) ->

	Part = "part=snippet&fields=items(id(videoId))",						%% Partial Request: request only ID 'field' in the Snippet 'part'

	Q = lists:append("q=", apis_aux:format_keyword(HashTag)),				%% Handle case for multiple words HashTag
	io:format("Q is ~p~n", [Q]),

	MaxResults = "maxResults=" ++ integer_to_list(Count),					%% increase max results to 10

	% Handle case history_timestamp
	After = case HistoryTimestamp of
		%% Generate AFTER Time Parameter for normal search. Fetch only videos uploaded in the last 24-hours
		[] -> apis_aux:youtube_get_after_param();
		%% Generate BEFORE and AFTER time parameters for History search
		Timestamp -> apis_aux:youtube_get_after_before_params(Timestamp)
	end,

	Type = "type=video&videoCaption=closedCaption&videoEmbeddable=true",	%% filter only VIDEO 'resource type' that contain capion

	Key = "key=" ++ aux_functions:get_youtube_keys(),						%% API KEY parameter

	Url = ?SEARCH_URL ++ Part ++ "&" ++ Q ++ "&" ++ MaxResults ++ "&" ++ After ++ "&" ++ Type ++ "&" ++ Key,

	case httpc:request(Url) of 
		{ok, Result} -> 
			{_StatusLine, _Headers, Body} = Result,
            
            DecodedBody = jsx:decode(list_to_binary(Body)),

			case parser:extract(<<"items">>, DecodedBody) of
    			{found, Ids} -> 
    				CleanIds = parser:extract_youtube_ids(Ids),
    				VideoList = [video_search(X) || X <- CleanIds],									% GET a list of decoded Video 'resources' 
    				Results = [parser:parse_youtube_video(Y, HashTag) || Y <- VideoList],			% Return a list of parsed video items 
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

%% 	@doc Helper function for search/2 
%%	Sends a GET request using the Videos:list method for a given VideoId
video_search(VideoId) ->

    Part = "part=snippet,statistics,player,status",							%% request details and statistics 'fields'

    Id = "id=" ++ VideoId,													%% id parameter

    Key = "key=" ++ aux_functions:get_youtube_keys(),

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