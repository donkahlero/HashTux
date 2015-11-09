-module(main_flow_worker).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3]).
-export([start_link/0]).


%%% =======================================================
%%% PUBLIC API
%%% =======================================================

start_link() ->
	io:format("STARTING:miner_worker~n"),
	gen_server:start_link(?MODULE, [], []).


%%% =======================================================
%%% CALLBACK FUNCTIONS
%%% =======================================================


%% ========================================================
init([]) -> 
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

handle_info(_Msg, State) -> 
	io:format("Main_flow: received info too late."),
	{noreply, State}.


handle_call({search, Term, _Options}, From, State) -> 
	io:format("Term: ~p~n", [Term]),
	
	% Update the database with search term / session data for this request
	% ...
	% ...
	% ...
	
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
	
	%
	% Decide on some way to forward the cache to the request / miner code
	%
	
	% Make a miner call for the term
	{ok, MinerPid} = miner_server:search(Term, none),
	receive 
		{MinerPid, Y, Z} ->
			{reply, Y, State}
		after 15000 ->
			io:format("Miner timeout!", []),
			{reply, [], State}
	end.
	
	%io:format("MSG: ~p~n", [X]),
	%{reply, ok, State}.




