-module(miner_worker).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
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
handle_info(_Msg, S) ->
	{noreply, S}.


%% ========================================================
handle_cast({{Pid, _Ref}, Term, Options}, State) ->
	io:format("SEARCH TERM in worker: ~p~n", [Term]),
	Res = ig_search:search(Term),
	io:format("RETURNED RESULT from insta parsing: ~p~n", [Res]),
	Res1 = twitter_search:search_hash_tag(Term, []),
	io:format("RETURNED RESULT from twitter parsing: ~p~n", [Res1]),
	Pid ! {self(), Res, Options},
	io:format("FINISHED:worker [~p]~n", [self()]),
	{stop, normal, State}.	


%% ========================================================
handle_call({Term, Options}, _From, S) -> 
	{reply, {Term, Options}, S}.








