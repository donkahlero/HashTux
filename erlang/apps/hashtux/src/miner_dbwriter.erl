-module(miner_dbwriter).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0, stop/0]).
-export([write/1]).




%%%=============================================================================
%%% PUBLIC API
%%%=============================================================================


%%
%% @doc Starts the server.
%%
start_link() ->
	io:format("MINER_DBWRITER: Starting...~n"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% 
%% @doc Stops the server.
%%
stop() ->
	gen_server:cast(?MODULE, stop).


%%
%% @doc Writes to the database.
%%
write(Data) ->
	gen_server:cast(?MODULE, {write, Data}).




%%%=============================================================================
%%% CALLBACK FUNCTIONS
%%%=============================================================================


%%
%% @doc Handles initialisation of the server.
%%
init([]) ->
	{ok, []}.


%%
%% @doc Handles termination of the server.
%%
terminate(Reason, _State) ->
	io:format("MINER_DBWRITER: Stopping for reason: ~p~n", [Reason]),
	ok.


%%
%% @doc Handles code change.
%%
code_change(_PrevVersion, State, _Extra) ->
	{ok, State}.


%%
%% @doc Handles messages sent to the server.
%%
%%% When database write succesful.
handle_info({_Pid, true}, State) ->
	io:format("MINER_DBWRITER: Writing to database succesful~n"),
	{noreply, State};
%%% All other messages -> report unknown message.
handle_info(Msg, State) ->
	io:format("MINER_DBWRITER: Unknown message: ~p~n", [Msg]),
	{noreply, State}.


%%
%% @doc Handles casts to the server.
%%
%%% When write to database requested.
handle_cast({write, Data}, State) ->
	io:format("MINER_DBWRITER: Writing to database...~n"),
	gen_server:call(db_serv, {add_doc, Data}),
	{noreply, State};
%%% Stopping the server.
handle_cast(stop, State) ->
	{stop, normal, State};
%%% All other messages -> ignored.
handle_cast(Msg, State) ->
	io:format("MINER_DBWRITER: Unknown cast: ~p~n", [Msg]),
	{noreply, State}.


%%
%% @doc Handles calls to the server. No calls supported.
%%
handle_call(Request, _From, State) ->
	io:format("MINER_DBWRITER: Unknown call: ~p~n", [Request]),
	{noreply, State}.






