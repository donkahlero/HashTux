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
	{QsVal, _} = cowboy_req:qs_val(<<"tag">>, Req),
	
	Path2 = Path,
	
	io:format("URL: ~p~nPath: ~p~nQs: ~p~nValue of tag: ~p~n~n", [URL, Path2, Qs, QsVal]),
	
	{ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}							
    ], <<"dddd">>, Req),
	{ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.



%% ====================================================================
%% Internal functions
%% ====================================================================


