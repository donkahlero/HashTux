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
	Res = ig_search:search(Term, Options),
	Res1 = twitter_search:search_hash_tag(Term, []),
	FinalRes = Res ++ Res1,								
	Pid ! {self(), FinalRes, Options},
	io:format("FINISHED:worker [~p]~n", [self()]),
	gen_server:call(db_serv, {add_doc, FinalRes}),
	{stop, normal, State}.	


%% ========================================================
handle_call({Term, Options}, _From, S) -> 
	{reply, {Term, Options}, S}.








