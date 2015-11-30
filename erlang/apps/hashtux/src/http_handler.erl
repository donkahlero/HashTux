%% @author jerker
%% @doc 
%
% This module handles requests from the frontend web server.
% It logs user habit detals about the request and then lets main_flow modules take
% care of the request. It then returns the reply from main_flow to the web server.
%
% The request from the web server supplies a number of details about the user, session
% and client options.
%
% The code in http_handler and main_flow* is very agnostic about what is in the options or 
% user habit data. The only thing that matters is that we can get the request_type value 
% from the options.
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
% Notes on cowboy:
% URL: the full url, including http://
% {URL, _} =  cowboy_req:url(Req),	
% QueryString: all the query stuff after the ?
% {Qs, _} = cowboy_req:qs(Req),
%	
%	TODO: try/catch on malformed jsx?
%	TODO: check cURL timeout / load balancing on PHP
%	TODO: Check spaces in search terms


-module(http_handler).
-behaviour(cowboy_http_handler).

%% ====================================================================
%% API functions
%% ====================================================================

-export([init/3, handle/2, terminate/3]).


init(_Type, Req, []) ->
   {ok, Req, undefined}.



handle(Req, State) ->	
	% Extract the request path (a string starting with /, we then remove this character)
	{Path, _} = cowboy_req:path(Req),
	[_ | Term] = binary:bin_to_list(Path),
	io:format("~nhttp_handler: Handling term ~p~n", [Term]),
	
	% Get the request body - will be json [options, user_habit_data]
	% Pattern match the distinct sublists against the decoded JSON.
	% Force JSX to turn keys in key-value pairs to atoms.
	{ok, RequestBody, _Req} = cowboy_req:body(Req),	
	[Options, UserHabitData] = jsx:decode(RequestBody, [{labels, atom}]),

	% Store user habit data - includes the options
	user_habits:store(Term, Options, UserHabitData),
	
	% Send the search term, request type and the options to the main flow by making a call
	% to main flow server - get the PID of the worker back and wait for a reply from it
	io:format("http_handler: Options: ~p~n", [Options]),
	RequestType = aux:bin_to_atom(aux:get_value(request_type, Options)),
	{ok, HandlerPid} = gen_server:call(main_flow_server, {RequestType, Term, Options}),
	io:format("~nhttp_handler: Made main_flow_server call, received worker PID: ~p~n", [HandlerPid]),
	
	receive 
		{HandlerPid, Reply} -> 
			io:format("~nhttp_handler: Receved a reply from worker ~p handling term ~p."
					 ++ " Sending reply to client...~n", [HandlerPid, Term])
		after 20000 ->
			io:format("~nhttp_handler: Timeout from worker~p handling term ~p~n", [HandlerPid, Term]),
			Reply = []
		end,	
	
	% Send the reply from the main flow call, which should be
	% a list of social media posts. We encode it with jsx and send it out.
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}],
								  jsx:encode(Reply), Req), 
	{ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.



%% ====================================================================
%% Internal functions
%% ====================================================================


