-module(miner_worker).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0]).


%%% =======================================================
%%% PUBLIC API
%%% =======================================================

start_link() ->
	io:format("starting miner_worker~n"),
%	process_flag(trap_exit, true),	
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
handle_info(Msg, S) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, S}.


%% ========================================================
handle_cast({{Pid, _Ref}, Term, Options}, State) ->
	timer:sleep(10),
	Pid ! {self(), Term, Options},
	io:format("stopping worker ~p~n", [self()]),
	{stop, normal, State}.	


%% ========================================================
handle_call({Term, Options}, _From, S) -> 
	{reply, {Term, Options}, S}.









