-module(miner_dbwriter).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0, stop/0]).
-export([write/1]).




%%%=============================================================================
%%% PUBLIC API
%%%=============================================================================


start_link() ->
	io:format("MINER_DBWRITER: Starting...~n"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
	gen_server:cast(?MODULE, stop).


write(Data) ->
	gen_server:cast(?MODULE, {write, Data}).




%%%=============================================================================
%%% CALLBACK FUNCTIONS
%%%=============================================================================


init([]) ->
	{ok, []}.


terminate(Reason, _State) ->
	io:format("MINER_DBWRITER: Stopping for reason: ~p~n", [Reason]),
	ok.


code_change(_PrevVersion, State, _Extra) ->
	{ok, State}.


handle_info(Msg, State) ->
	io:format("MINER_DBWRITER: Unknown message: ~p~n", [Msg]),
	{noreply, State}.


handle_cast({write, Data}, State) ->
	io:format("MINER_DBWRITER: Writing to database...~n"),
	gen_server:call(db_serv, {add_doc, [Data]}),
	{noreply, State};
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(Msg, State) ->
	io:format("MINER_DBWRITER: Unknown cast: ~p~n", [Msg]),
	{noreply, State}.


handle_call(Request, _From, State) ->
	io:format("MINER_DBWRITER: Unknown call: ~p~n", [Request]),
	{reply, {undef_call, Request}, State}.






