-module(ig_search).

-export([search/1]).

%-define(CL_ID, 43956b9093074affb3012e9315533cf0).
%-define(CL_SECRET, b054932ad0164623bf4c552b9250e392).
%-define(CODE, 0cfd381166794097933883eeddd687e8).
-define(ACCESS_TOKEN, "2253641420.43956b9.2ce7ad96329647ffa756501b24fbed3e").

-define(URL, "https://api.instagram.com/v1/tags/").
-define(TAIL, "/media/recent?count=2&access_token=").


search(Term) ->
	Url = ?URL ++ Term ++ ?TAIL ++ ?ACCESS_TOKEN,
	io:format("URL for IG: ~p~n", [Url]),
	case httpc:request(Url) of
		{ok, Result} -> 
			io:format("RESULT RECEIVED FROM IG~n"),
			{_StatusLine, _Headers, Body} = Result,
			DecodedRes = jsx:decode(list_to_binary(Body)),
			io:format("DECODED RESULT from IG: ~p~n", [DecodedRes]),
			case extract(<<"data">>, DecodedRes) of
				{found, DataList} ->
					print_decoded_res(DataList);
				not_found ->
					io:format("DATA LIST not found~n")
			end,	
			search_parser:parse_insta(DecodedRes);
		{error, Reason} ->
			io:format("REQUEST FAILED for reason: ~p~n", [Reason])
	end.



print_decoded_res([]) 	 -> ok;
print_decoded_res([H|T]) -> 
 	print_details(H),
	print_decoded_res(T).



print_details(L) ->
	case extract(<<"type">>, L) of
		{found, V} -> 
			io:format("TYPE: ~p~n", [binary_to_list(V)]);
		not_found  ->
			io:format("TYPE not found~n")
	end,

	case extract(<<"images">>, L) of
		{found, ImageList} ->
			case extract(<<"standard_resolution">>, ImageList) of
				{found, ImageSpec} ->
					case extract(<<"url">>, ImageSpec) of
						{found, Url} ->
							io:format("IMAGE URL: ~p~n", [binary_to_list(Url)]);
						not_found 	 ->
							io:format("IMAGE URL not found~n")
					end;
				not_found ->
					io:format("STANDARD RES missing~n")
			end;
		not_found ->
			io:format("IMAGES NOT FOUND~n")
	end.




extract(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		{_, TupleList} -> {found, TupleList};
		false 				 -> not_found
	end.
