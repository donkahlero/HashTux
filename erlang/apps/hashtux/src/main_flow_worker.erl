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


handle_cast({search, SourcePID, Term, Options}, State) -> 
	io:format("main_flow_worker: Term: ~p~nmain_flow_worker: Options:~p~n", [Term, Options]),
	
	% Structure:
	% If search, treshold value is ~30 or so
	% If update, treshold value is ~5 or so	
	% Ask DB for matching posts from the last minute or so is cached, provided these options -
	%	the DB should only return this if the post count is threshold or more! 
	% If we get something, simply return this
	% If not (empty list), mine as usual and send back the reply from mining. 
	% This last point will later be expanded into:
	%	Get what was last cached on this term
	%	Provide this to the miners so they themselves can adjust the mining accordingly
	%	Send back the results after mining.
	
	% Make a database call for the term
	%Ref = gen_server:call(db_serv, {get_cont, Term}),
	%receive 
	%	{Ref, [{<<"error">>,<<"not_found">>},{<<"reason">>,<<"missing">>}]} ->
	%		{reply, "Nothing found!", State};
	%	{Ref, Res} ->
	%		{reply, Res, State}
	%	after 1000 ->
	%		{reply, "DB timeout!", State}
	%end.
	
	% Make a miner call for the term
	{ok, MinerPid} = miner_server:search(Term, Options),

	% Wait for the reply and then send this to whoever made the request in the
	% first place, presumably some process running the http_handler...
	receive 
		{MinerPid, Y} ->
			io:format("main_flow_worker: Received reply from miner server~n", []),
			SourcePID ! {self(), Y}
		after 15000 ->
			io:format("main_flow_worker: Miner timeout!~n", []),
			SourcePID ! {self(), []}
	end,

	% Stop this worker 
	{stop, normal, State}.
	




