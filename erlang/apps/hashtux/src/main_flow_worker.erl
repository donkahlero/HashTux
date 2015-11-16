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
				handle_info/2, handle_cast/2]).
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

% Structure:
	% If not (empty list), mine as usual and send back the reply from mining. 
	% This last point will later be expanded into:
	%	Get what was last cached on this term
	%	Provide this to the miners so they themselves can adjust the mining accordingly
	%	Send back the results after mining.	

handle_cast({heartbeat, SourcePID, Term, Options}, State) -> 
	% Heartbeat from client means miners should cache data for this
	% request, that the client can "pick up" later. No data should be 
	% returned to the client right now, and we don't wait for a reply.
	miner_server:search(Term, Options),
	
	% For simplicity, we just return [] to the using code
	SourcePID ! {self(), []}, 	

	% Stop this worker 
	{stop, normal, State};
	
handle_cast({RequestType, SourcePID, Term, Options}, State) -> 
	io:format("main_flow_worker: Term: ~p~nmain_flow_worker: "
			 ++ "Options:~p~n", [Term, Options]),
	
	% Check type 1 cache for recent data.
	% Then take approperiate action (call miners if needed) and send a reply
	% to whoever made the request in the first place, presumably some process
	% running the http_handler...
	CacheResult = cache_type1_query(Term, RequestType, Options),
	case CacheResult of
		no_miner_res -> 
			% The miners have executed lately but found nothing, return []
			SourcePID ! {self(), []};
		[] -> 
			% Means the miners have NOT executed - make a miner request
			SourcePID ! {self(), miner_query(Term, Options)};
		List ->
			% Some results were found, return them
			SourcePID ! {self(), List}
	end,

	% Stop this worker 
	{stop, normal, State}.



miner_query(Term, Options) ->
	% Make a miner call for the term
	{ok, MinerPid} = miner_server:search(Term, Options),

	% Wait for the reply and return it.
	receive 
		{MinerPid, Result} ->
			io:format("main_flow_worker: Received reply from miner server~n", []),
			Result
		after 15000 ->
			% Timeout -> return empty list
			io:format("main_flow_worker: Miner timeout!~n", []),
			[]
	end.


% Checks to see if there is anything cached in the DB very recently
% (such as the last minute) by the heartbeat mechanism 
cache_type1_query(Term, RequestType, Options) ->
	% Add some constraints to the DB request,
	% time window is from 60 seconds ago to now
	EndTime = dateconv:get_timestamp(),
	StartTime = EndTime - 60,
	Options2 = Options ++ {timeframe, StartTime, EndTime} ++ {limit, 50},
	
	Ref = gen_server:call(db_serv, {get_posts, Term, Options2}),
	receive 
		{Ref, Result} ->
			% Just return the result to the using code.
			Result
		after 10000 ->
			[]
	end.



