-module(ig_search).

-export([search/1]).

%-define(CL_ID, 43956b9093074affb3012e9315533cf0).
%-define(CL_SECRET, b054932ad0164623bf4c552b9250e392).
%-define(CODE, 0cfd381166794097933883eeddd687e8).
-define(ACCESS_TOKEN, "2253641420.43956b9.2ce7ad96329647ffa756501b24fbed3e").

-define(URL, "https://api.instagram.com/v1/tags/").
-define(TAIL, "/media/recent?access_token=").


search(Term) ->
	Url = ?URL ++ Term ++ ?TAIL ++ ?ACCESS_TOKEN,
	io:format("URL for IG: ~p~n", [Url]),
	case httpc:request(Url) of
		{ok, Result} -> 
			io:format("RESULT RECEIVED FROM IG~n"),
			io:format("Request: ~p~n", [Result]);
		{error, Reason} ->
			io:format("REQUEST FAILED for reason: ~p~n", [Reason])
	end.
