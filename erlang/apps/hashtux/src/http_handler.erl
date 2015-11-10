%% @author jerker
%% @doc 
%
% This module handles requests from the Apache server handling the frontend.
%
% The PHP request supplies a number of details about
% the user, session and client options.
%
% (It would actually be possible to put most of this into JSON already in the 
% PHP code, reducing the amount of logic around this on the erlang backend. 
% This is a good candidate for refactoring later if we have time. However then
% either the client would have to care about what we deem as user habit related
% and what we see as only relevant as options, OR we would have to separate
% into two lists on the server anyway, so it's not a magical wand solution that
% instantly makes everything superbeautiful)
%
% User habit related:
% 	Timeout
%	Session ID
%	IP address
% 	Language (hmm is this a part of user agent or not? hmmm)
%	User agent
% Technical:
%	Limit (post count in response) (also stored as user habit right now)
%	Services (t, i, y) (also stored as user habit right now)
%	Resolution (high, low, default)
%
% Other notes:
% URL: the full url, including http://
% {URL, _} =  cowboy_req:url(Req),	
% QueryString: all the query stuff after the ?
% {Qs, _} = cowboy_req:qs(Req),
%
% 	TODO: Create a function that takes req as an argument
%	and returns an option tuple to be sent to main flow
% 	TODO: Handle resolution, with default value handled
%	TODO: check cURL timeout / load balancing on PHP
%	TODO: Check spaces in search terms
%	TODO: rememeber client side option text above pic on/off - unrelated here but putting it down not to forget	
	


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
			
	% Extract the path (the search term, in our protocol)
	[_ | Term] = binary:bin_to_list(Path),
	
	% "Debug" output
	io:format("~nNow handling term: ~p~n", [Term]),
	
	% Store user habit data 	
	user_habits:store(Term, Req),
	% Extract options
	Options = http_option_parser:parse_options(Req),
	
	% Send the search term and the options to the main flow by making a call to 
	% main flow server - get the PID of the worker back and wait for a reply from it
	{ok, HandlerPid} = gen_server:call(main_flow_server, {search, Term, Options}),
	io:format("~nWorker PID: ~p~n", [HandlerPid]),
	
	receive 
		{HandlerPid, Reply} -> 
			io:format("~nReceved a reply from worker ~p~n", [HandlerPid]),
			ok
		after 20000 ->
			io:format("~nTimeout from worker~p~n", [HandlerPid]),
			Reply = []
		end,
	
	io:format("Main flow returned from handling ~p~n", [Term]),
	
	% Send the reply from the main flow call, which should be
	% a list of social media posts. We encode it with jsx and send it out.
	{ok, Req2} = cowboy_req:reply(200, [
        {<<"content-type">>, <<"application/json">>}							
    ], jsx:encode(Reply), Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.



%% ====================================================================
%% Internal functions
%% ====================================================================


