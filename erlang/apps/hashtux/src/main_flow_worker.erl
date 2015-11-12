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
	io:format("Started main flow_worker~n",  []),
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

handle_info(Msg, State) -> 
	io:format("Main_flow worker: received info too late: ~p~n", [Msg]),
	{noreply, State}.


handle_cast({search, SourcePID, Term, Options}, State) -> 
	io:format("Term: ~p~nOptions:~p~n", [Term, Options]),
	
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
	{ok, MinerPid} = miner_server:search(Term, Options),
	receive 
		{MinerPid, Y} ->
			io:format("Miner reply~n", []),
			SourcePID ! {self(), Y}
		after 15000 ->
			io:format("Miner timeout!~n", []),
			SourcePID ! {self(), []}
	end,
	%SourcePID ! {self(), []},

	{noreply, State}.
	
	%io:format("MSG: ~p~n", [X]),
	%{reply, ok, State}.




