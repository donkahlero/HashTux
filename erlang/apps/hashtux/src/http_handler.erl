%% @author jerker
%% @doc 
%
% The PHP request supplies a number of details about
% the user, session and client options.
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

% 	TODO: Create a function that takes req as an argument
%	and returns an option tuple to be sent to main flow
% 	TODO: Handle resolution, with default value handled
%	TODO: check cURL timeout / load balancing
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
	
	% URL: the full url, including http://
	%{URL, _} =  cowboy_req:url(Req),	
	% QueryString: all the query stuff after the ?
	%{Qs, _} = cowboy_req:qs(Req),
		
	% Extract the path (the search term)
	[_ | Term] = binary:bin_to_list(Path),
	
	% "Debug" output
	io:format("~nNow handling term: ~p~n", [Term]),
	
	% Store user habit data 	
	user_habits:store(Term, Req),
	
	% Send the search term and the options to the main flow 
	%Reply = try gen_server:call(main_flow, {search, Term}, 20000)
	%	catch _ -> []
	%end,

	% Make a call to main flow server - get the PID of the worker back
	% and wait for a reply from it

	{ok, HandlerPid} = gen_server:call(main_flow_server, {search, Term, none}, 20000),
	io:format("~nWorker PID: ~p~n", [HandlerPid]),
	
	receive 
		{HandlerPid, Reply} -> 
			io:format("~nReceved from worker~n", []),
			ok;
		X -> 
			io:format("~nReceved from ? ~p~n", [X]),
			Reply = []
		after 20000 ->
			io:format("~nTimeout from worker~n", []),
			Reply = []
		end,
	
	io:format("Main flow returned from handling ~p~n", [Term]),
	
	% io_lib:format does about the same thing as io:format but returns a string instead of printing
	%Body = io_lib:format("Welcome to HashTux!~n~nHashtag for mining: ~p~nSessionID: ~p~nIP Address: ~p~nLanguage: ~p~nUser Agent: ~p~nResult: ~p~n~n",
	%					 [Term, SessionID, IPAddress, Language, UserAgent, Reply]),	
	
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


