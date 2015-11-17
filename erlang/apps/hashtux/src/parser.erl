-module(parser).

-export([extract/2, atom_to_binarystring/1]).
-export([extract_youtube_ids/1, parse_youtube_video/2, clean_result/1]).									% YOUTUBE Functions
-export([is_content_type/2]).																				% TWITTER Functions

% ****************************************
% @doc A parser for decoded JSON Items
% ****************************************

% the value of a key from decoded JSON message.
extract(K, L) ->
  case lists:keyfind(K, 1, L) of
    {_, M} -> {found, M};
    false  -> not_found
  end.

% @doc extract a field from a parent node. 
% return value if field was found, null if it was not found 
extract_from_node(_Field, null) -> null;
extract_from_node(Field, Node) -> 
	case extract(Field, Node) of
		{found, X} -> X; 
		not_found -> null
    end.

% ****************************************
% PARSE YOUTUBE decoded JSON
% ****************************************

% @ doc extract the Id from a list of decoded Youtube feeds returned by a Keyword search 
extract_youtube_ids(List) ->
	extract_ids(List, []).

extract_ids([], Result) -> Result;
extract_ids([H|T], Result) ->
	NewResult = Result ++ [extract_id(H)],
	extract_ids(T, NewResult).

extract_id([{<<"id">>, [{<<"videoId">>, VideoId}]}]) -> binary_to_list(VideoId);
extract_id(_) -> parser_error.

% Remove empty fields from Final Result (i.e. removes fields that returned null)
clean_result(L) -> [X || X <- L, has_null_value(X) == false].

has_null_value({_, null}) -> true;
has_null_value({_, _}) -> false.

% @ convert a decoded youtube Video resource to internal representation
parse_youtube_video(Video, HashTag) -> 
	
	%% Register the time the document was sent to DB
    Timestamp = dateconv:get_timestamp(),

    case extract(<<"items">>, Video) of
		{found, [Items]} -> 
			
			Id = extract_from_node(<<"id">>, Items),

			Snippet = extract_from_node(<<"snippet">>, Items),

			PubDate = dateconv:youtube_to_epoch(binary_to_list(extract_from_node(<<"publishedAt">>, Snippet))),		%% convert to EPOCH

			Description = extract_from_node(<<"description">>, Snippet),

			Tags = extract_from_node(<<"tags">>, Snippet),

			Statistics = extract_from_node(<<"statistics">>, Items),

			ViewCount = extract_from_node(<<"viewCount">>, Statistics),

			LikeCount = extract_from_node(<<"likeCount">>, Statistics),

			{Id, PubDate, Description, ViewCount, LikeCount, Tags};

		not_found -> 
			
			Id = null,
			PubDate = null,
			Description = null,
			ViewCount = null,
			LikeCount = null,
			Tags = null,

			{Id, PubDate, Description, ViewCount, LikeCount, Tags}
    end,

    % NOTE: add 'Clean Result'
    A = [{<<"search_term">>, list_to_binary(HashTag)}, {<<"service">>, <<"youtube">>}, {<<"insert_timestamp">>, Timestamp}, {<<"timestamp">>, PubDate}, {<<"content_type">>, <<"video">>}, {<<"service_id">>, Id}, {<<"text">>, Description}, {<<"view_count">>, ViewCount}, {<<"likes">>, LikeCount}, {<<"tags">>, Tags}],

    % return clean result
    clean_result(A).

% ****************************************

% @doc Convert an atom to binary_string
atom_to_binarystring(Atom) ->
	list_to_binary(atom_to_list(Atom)).

% ****************************************
% PARSE TWITTER decoded JSON
% ****************************************

% @doc Tells if a given Tweet feed is of one of the types (content-types: text, image, video) included in the list TypeFilter.
is_content_type(Status, TypeFilter) -> 

	Extracted_CT = binary_to_list(extract_from_node(<<"content_type">>, Status)),

	lists:member(Extracted_CT, TypeFilter).