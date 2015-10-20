%% @author jerker
%% @doc @todo Add description to http_handler.


-module(http_handler).
-behaviour(cowboy_http_handler).

%% ====================================================================
%% API functions
%% ====================================================================

-export([init/3, handle/2, terminate/3]).


init(_Type, Req, []) ->
   {ok, Req, undefined}.



handle(Req, State) ->
	% URL: the full url, including http://
	{URL, _} =  cowboy_req:url(Req),
	
	% Path: the path, starting with /
	{Path, _} = cowboy_req:path(Req),
	
	% QueryString: all the query stuff after the ?
	{Qs, _} = cowboy_req:qs(Req),
	
	% Qs_val: can be used with an atom to request a particular value
	{QsVal, _} = cowboy_req:qs_val(<<"option1">>, Req, <<"default">>),
	
	% Remove the leading slash from the path
	[_ | Path2] = binary:bin_to_list(Path),
	
	% "Debug" output
	io:format("~nURL requested: ~p~nPath: ~p~nQs: ~p~nValue of tag: ~p~n~n",
			  [binary:bin_to_list(URL), Path2, binary:bin_to_list(Qs), 
			   binary:bin_to_list(QsVal)]),
	
	% io_lib:format does about the same thing as io:format but returns a string
	% instead of printing
	Body = io_lib:format("Welcome to HashTux!~n~nURL requested: ~p~nHashtag for mining: ~p~nQs: ~p~nValue of option \"option1\": ~p~n~n",
						 [binary:bin_to_list(URL), Path2, binary:bin_to_list(Qs), 
			   				binary:bin_to_list(QsVal)]),
	
	
	{ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}							
    ], binary:list_to_bin(Body), Req),
	{ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.



%% ====================================================================
%% Internal functions
%% ====================================================================


