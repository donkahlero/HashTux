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
	
	% The PHP document has supplied a number of details about
	% the user, session and client options:
	%	Session ID
	%	IP address
	% 	Language (hmm is this a part of user agent or not? hmmm)
	%	User agent
	%	Limit (post count in response)
	%	Services (t, i, y)

	% Qs_val: can be used with an atom to request a particular value
	{SessionID, _} = cowboy_req:qs_val(<<"session_id">>, Req, <<"unknown">>),
	{IPAddress, _} = cowboy_req:qs_val(<<"ip_address">>, Req, <<"unknown">>),
	{Language, _} = cowboy_req:qs_val(<<"language">>, Req, <<"unknown">>),
	{UserAgent, _} = cowboy_req:qs_val(<<"user_agent">>, Req, <<"unknown">>),
	{Limit, _} = cowboy_req:qs_val(<<"limit">>, Req, <<"unknown">>),
	{Services, _} = cowboy_req:qs_val(<<"services">>, Req, <<"unknown">>),
	
	% Remove the leading slash from the path
	[_ | Term] = binary:bin_to_list(Path),
	
	% 
	% Send the search term and the options to the main flow
	% 
	Reply = gen_server:call(main_flow, {search, Term}),

	% "Debug" output
	io:format("~nURL requested: ~p~nPath: ~p~nQs: ~p~nSessionID: ~p~nResult: ~p~n~n",
			  [binary:bin_to_list(URL), Term, binary:bin_to_list(Qs), Reply, 
			   binary:bin_to_list(SessionID)]),
	
	% io_lib:format does about the same thing as io:format but returns a string
	% instead of printing
	Body = io_lib:format("Welcome to HashTux!~n~nURL requested: ~p~nHashtag for mining: ~p~nQs: ~p~nSessionID: ~p~nResult: ~p~n~n",
						 [binary:bin_to_list(URL), Term, binary:bin_to_list(Qs), SessionID, Reply]),	
	
	{ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}							
    ], binary:list_to_bin(Body), Req),
	{ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.



%% ====================================================================
%% Internal functions
%% ====================================================================


