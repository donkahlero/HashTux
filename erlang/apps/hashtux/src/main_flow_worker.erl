%% @author jerker
%% @doc 
%
% The main flow workers are responsible for processing a request correctly once it is
% received from the HTTP interface, by contacting the DB and miners as approperiate,
% and then returning a response to the HTTP interface to be returned to the Apache
% server. 
% (Based the server-worker structure on Ivos structure for miner server and miner workers.)

-module(main_flow_worker).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_cast/2, handle_call/3]).
-export([start_link/0]).


%%% =======================================================
%%% PUBLIC API
%%% =======================================================

start_link() ->
	gen_server:start_link(?MODULE, [], []).


%%% =======================================================
%%% CALLBACK FUNCTIONS
%%% =======================================================


%% ========================================================
init([]) -> 
	io:format("main_flow_worker: started, PID: ~p~n",  [self()]),
	{ok, []}.


%% ========================================================
%terminate({'EXIT', _From, _Reason}, _State) ->
% 	io:format("received exit signal in worker~n"),
%	ok;
terminate(_Reason, _State) ->
	ok.


%% ========================================================
code_change(_PrevVersion, _State, _Extra) -> 
	ok.


%% ========================================================

handle_info(Msg, State) -> 
	io:format("main_flow_worker: Received info too late: ~p~n", [Msg]),
	{noreply, State}.

%% ========================================================

handle_call(_Msg, _From, State) ->
	{noreply, State}.


%% ========================================================

% Cast: stats
handle_cast({stats, SourcePID, Term, Options}, State) ->
	io:format("main_flow_worker: Stats request: ~p~n", [Term]),
	
	% Return the result of the helper method to the requesting PID
	SourcePID ! {self(), stats_query(Term, Options)},

	% Stop this worker 
	{stop, normal, State};

% Cast: heartbeat
handle_cast({heartbeat, SourcePID, Term, Options}, State) -> 
	% Heartbeat from client means miners should cache data for this
	% request, that the client can "pick up" later. No data should be 
	% returned to the client right now, and we don't wait for a reply.
	miner_cast_only(Term, Options),
	io:format("main_flow_worker: Heartbeat for term ~p~n", [Term]),
	
	% For simplicity, we just return [] to the using code
	SourcePID ! {self(), []}, 	

	% Stop this worker 
	{stop, normal, State};

% Cast: search or update
handle_cast({_RequestType, SourcePID, Term, Options}, State) -> 
	io:format("main_flow_worker: Term: ~p~nmain_flow_worker: "
			 ++ "Options:~p~n", [Term, Options]),
	
	% Check type 1 cache for recent data.
	% Then take approperiate action (call miners if needed) and send a reply
	% to whoever made the request in the first place, presumably some process
	% running the http_handler...
	CacheResult = cache_query(Term, Options),
	case CacheResult of
		no_miner_res ->
			% The miners have executed lately but found nothing, return []
			io:format("main_flow_worker: Miners ran recently but no results.~n"),
			SourcePID ! {self(), []};
		[] -> 
			% Means the miners have NOT executed - make a miner request
			io:format("main_flow_worker: Miners haven't run recently.~n"),
			SourcePID ! {self(), miner_query(Term, Options)};
		List ->
			% Some results were found, return them
			io:format("main_flow_worker: " ++
						  "Miners have run recently, returning cached data~n"),
			SourcePID ! {self(), List}
	end,

	% Stop this worker 
	{stop, normal, State}.


%% ========================================================


% Helper function for querying the DB for statistical user habit data.
stats_query(Term, Options) ->
	Ref = gen_server:call(db_serv, {get_stats, Term, Options}),
	receive 
		{Ref, Result} ->
			% Just return the result to the using code.
			Result
		after 60000 ->
			[]
	end.

% Helper functions that checks if there is anything cached in the DB very recently
% (such as the last minute) by the heartbeat mechanism 
cache_query(Term, Options) ->
	% TODO: Good candidate for storing in a config file on refactoring
	% The amount of seconds for which we consider cached data to still
	% be up to date.
	CacheTimeWindow = 60,
	
	% Add some constraints to the DB request,
	% time window is from 60 seconds ago to now
	EndTime = dateconv:get_timestamp(),
	StartTime = EndTime - CacheTimeWindow,
	Options2 = Options ++ [{timeframe, StartTime, EndTime}, {limit, 50}],
	
	Ref = gen_server:call(db_serv, {get_posts, Term, Options2}),
	receive 
		{Ref, Result} ->
			% Just return the result to the using code.
			Result
		after 10000 ->
			[]
	end.




%
%
% NOTE,
% Ivo, the functions below could be moved to another module that makes it
% transparent to the using code in this module whether we use a local miner
% or remote one. Of course there are challenges, such as whether we keep track
% of where the last heartbeat was last redirected, and so on. But how do we then
% identify clients? Hmm. 
%
%

% Helper function used by the heartbeat mechanism to trigger precaching.
% Make sure that the options include reqeust_type: "heartbeat" 
% so the miner server won't try to send anything back to this process!
miner_cast_only(Term, Options) ->	
	miner_server:search(Term, Options).

% Helper function for sending a miner request. Returns results.
miner_query(Term, Options) ->
	% Make a miner call for the term
	Reply =  miner_server:search(Term, Options),
	case Reply of
		no_alloc ->
			% Miner server too busy
			no_alloc; 
		{ok, MinerPid} -> 
			% Wait for the reply and return it.
			receive 
				{MinerPid, Result} ->
					io:format("main_flow_worker: Received reply from miner server~n", []),
					Result
				after 15000 ->
					% Timeout -> return empty list
					io:format("main_flow_worker: Miner timeout!~n", []),
					[]
			end
	end.





