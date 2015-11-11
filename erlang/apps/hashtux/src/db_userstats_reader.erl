
%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% %% @doc Initial database actions module
%% %% %% @version 0.1

-module(db_userstats_reader).

-behaviour(gen_server).

-export([start_link/0, stop/0, state/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DB, "hashtux_userstats/").

%% Public API

%% @doc Starts the server
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% @doc Calls a stop tp the server
stop(Module) ->
	gen_server:call(Module, stop).

stop() ->
	stop(self()).

state(Module) ->
	gen_server:call(Module, state).

state() ->
	state(self()).

%% Server implementation, a.k.a.: callbacks

init([]) ->
	{ok, []}.

%% @doc Handles the calls
handle_call(stop, _From, _State) ->
	{stop, normal, stopped, _State};

%% @doc Catches all calls
handle_call(_, _, _) ->
	error(badarth).

%% %% @doc Handels the cast which is the messages where we doing operations on.
handle_cast({get_hash_count, Rec}, State) ->
	Result = couch_operations:doc_get_mapreduce_cont(
		   ?DB ++ "_design/stat/_view/by_search_term?group=true" ),
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_popular_hash, Num,  Rec}, State) ->
	PHR = couch_operations:doc_get_mapreduce_cont(
		?DB ++ "_design/stat/_view/by_search_term?group=true" ),
	Result = db_filter:limit_result(Num, db_filter:order_by_value(PHR)),
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_popular_browsers, Rec}, State) ->
	Result = couch_operations:doc_get_cont(
		   ?DB ++ "_design/stat/_view/by_?key=\"" ),
	Rec ! {self(), Result},
	{stop, normal, State}.

%% @doc Handels the info (not used)
handle_info(_Info, _State) ->
	{noreply, _State}.

%% @doc Terminates the server
terminate(_Reason, _State) ->
	ok.
