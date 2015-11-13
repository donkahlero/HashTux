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


% Options (appended at the end of user habit data as well before sent to db)
% 	language
%	limit (post count in response) (also stored as user habit right now)
%	services (twitter, instagram, youtube) (also stored as user habit right now)
%	content_type ()
%
% User habit related:
% 	timetamp
%	session_id
%	ip_address
%	platform
%	browser
%	browserversion
%
%
% Notes on cowboy:
% URL: the full url, including http://
% {URL, _} =  cowboy_req:url(Req),	
% QueryString: all the query stuff after the ?
% {Qs, _} = cowboy_req:qs(Req),
%
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
	
	% Extract the request path, starting with /, then remove this first character
	{Path, _} = cowboy_req:path(Req),
	[_ | Term] = binary:bin_to_list(Path),
	io:format("~nNow handling term: ~p~n", [Term]),
	
	% Get the request body - will be json [options, user_habit_data]
	{ok, RequestBody, _Req} = cowboy_req:body(Req),
	io:format("~nReceived body: ~n~p~n", [RequestBody]),

	% Pattern match the distinct sublists against the decoded JSON.
	% Force JSX to turn keys in key-value pairs to atoms.
	[Options, UserHabitData] = jsx:decode(RequestBody, [{Labels, atom}]),

	% Extract options from request
	%Options = http_option_parser:parse_options(Req),
	
	% Store user habit data - includes the options
	user_habits:store(Term, Options, UserHabitData),


	

	
	
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


