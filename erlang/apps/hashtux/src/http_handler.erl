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
	% Path: the path, starting with /
	{Path, _} = cowboy_req:path(Req),
	
	% URL: the full url, including http://
	%{URL, _} =  cowboy_req:url(Req),	
	% QueryString: all the query stuff after the ?
	%{Qs, _} = cowboy_req:qs(Req),
	
	
	%
	%	TODO: Create a module that takes req as an argument,
	%	extracts the relevant data, packs it into a jsx object
	%	and sends it to the DB for logging
	%
	% 	TODO: Create a FUNCTION that takes req as an argument
	%	and returns an option tuple to be sent to main flow
	%
	% The PHP document has supplied a number of details about
	% the user, session and client options:
	%	Session ID
	%	IP address
	% 	Language (hmm is this a part of user agent or not? hmmm)
	%	User agent
	%	Limit (post count in response)
	%	Services (t, i, y)

	% Qs_val: can be used with an atom to request a particular value
	%{SessionID, _} = cowboy_req:qs_val(<<"session_id">>, Req, <<"unknown">>),
	%{IPAddress, _} = cowboy_req:qs_val(<<"ip_address">>, Req, <<"unknown">>),
	%{Language, _} = cowboy_req:qs_val(<<"language">>, Req, <<"unknown">>),
	%{UserAgent, _} = cowboy_req:qs_val(<<"user_agent">>, Req, <<"unknown">>),
	%{Limit, _} = cowboy_req:qs_val(<<"limit">>, Req, <<"unknown">>),
	%{Services, _} = cowboy_req:qs_val(<<"services">>, Req, <<"unknown">>),
	
	% Remove the leading slash from the path
	[_ | Term] = binary:bin_to_list(Path),
	
	% 
	% Send the search term and the options to the main flow
	% 
	Reply = gen_server:call(main_flow, {search, Term}),

	% "Debug" output
	io:format("~nTerm: ~p~nResult: ~p~n~n",
			  [Term, Reply]),
	
	% io_lib:format does about the same thing as io:format but returns a string
	% instead of printing
	%Body = io_lib:format("Welcome to HashTux!~n~nHashtag for mining: ~p~nSessionID: ~p~nIP Address: ~p~nLanguage: ~p~nUser Agent: ~p~nResult: ~p~n~n",
	%					 [Term, SessionID, IPAddress, Language, UserAgent, Reply]),	
	
	%
	% Let's just send the reply from the main flow call, which should in turn
	% return the result of the search (as a json string)
	%
	Body = Reply,
	
	{ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"application/json">>}							
    ], binary:list_to_bin(Body), Req),
	{ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.



%% ====================================================================
%% Internal functions
%% ====================================================================


