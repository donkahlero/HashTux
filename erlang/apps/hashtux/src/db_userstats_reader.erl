%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% %% @doc A worker that handles the reading from the db for the userstats
%% %% %% @version 0.1

%% -----------------------------------------------------------------------------
%% | Sprint 4                                                                  |
%% | Version 0.1                                                               |
%% | This gen_server will work as a worker and handle the different messages   |
%% | from the sup for collecting  user statistics. Basic functionality is done,|
%% | it can read from the db and use mapreduce functions for getting the corre-|
%% | ct data.                                                                  |
%% -----------------------------------------------------------------------------

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
handle_cast({get_stats, get_search_term, Options, Rec}, State) ->
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_search_term?" ++	
			   			    db_options_handler:pre_search_opt(db_options_handler:order_options(Options)) ++ "&group=true"),
	Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
	Opt = db_options_handler:order_options(Options),
	LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt, {Atom, Limit} == {limit, Limit}], Result),
	Rec ! {self(), LR},
	{stop, normal, State};

handle_cast({get_stats, get_browser, Options, Rec}, State) ->
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_browser?" ++
						    db_options_handler:pre_search_opt(db_options_handler:order_options(Options)) ++ "&group=true"),
	Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
	Opt = db_options_handler:order_options(Options),
	LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt, {Atom, Limit} == {limit, Limit}], Result),
	Rec ! {self(), LR},
	{stop, normal, State};

handle_cast({get_stats, get_language, Options, Rec}, State) -> 
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_language?" ++  
						    db_options_handler:pre_search_opt(db_options_handler:order_options(Options)) ++ "&group=true"),
	Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
	Opt = db_options_handler:order_options(Options),
	LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt, {Atom, Limit} == {limit, Limit}], Result),
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_stats, get_platform, Options, Rec}, State) ->
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_platform?" ++
						   db_options_handler:pre_search_opt(db_options_handler:order_options(Options)) ++ "&group=true"),
	Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
	Opt = db_options_handler:order_options(Options),
	LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt, {Atom, Limit} == {limit, Limit}], Result),
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_stats, get_browser_version, Options, Rec}, State) ->
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_browser_version?" ++ 
						   db_options_handler:pre_search_opt(db_options_handler:order_options(Options)) ++ "&group=true"),
	Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
	Opt = db_options_handler:order_options(Options),
	LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt, {Atom, Limit} == {limit, Limit}], Result),
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_stats, get_platform_browser, Options, Rec}, State) ->
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_platform_browser?" ++ 
						   db_options_handler:pre_search_opt(db_options_handler:order_options(Options)) ++ "&group=true"),
	Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
	Opt = db_options_handler:order_options(Options),
	LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt, {Atom, Limit} == {limit, Limit}], Result),
	Rec ! {self(), Result},
	{stop, normal, State}.


%% @doc Handels the info (not used)
handle_info(_Info, _State) ->
	{noreply, _State}.

%% @doc Terminates the server
terminate(_Reason, _State) ->
	ok.

