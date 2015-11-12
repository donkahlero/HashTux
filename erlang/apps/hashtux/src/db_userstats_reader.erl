
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

handle_cast({get_posts, Rec}, State) ->
	Result = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/all"),
	Rec ! {self(), Result},
	{stop, normal, State};

%% %% @doc Handels the cast which is the messages where we doing operations on.
handle_cast({get_posts, get_hash_count, Rec}, State) ->
	Result = couch_operations:doc_get_mapreduce_cont(
		   ?DB ++ "_design/stat/_view/by_search_term?group=true" ),
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_posts, {get_popular_hash, Limit},  Rec}, State) ->
	PHR = couch_operations:doc_get_mapreduce_cont(
		?DB ++ "_design/stat/_view/by_search_term?group=true" ),
	Result = db_filter:limit_result(Limit, db_filter:order_by_value(PHR)),
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_posts, {get_requests_within_time, StartT, EndT}, Rec}, State) ->
	PHR = couch_operations:doc_get_mapreduce_cont(
		   ?DB ++ "_design/stat/_view/within_time?startkey=" ++ integer_to_list(StartT)  ++  "&endkey=" ++ integer_to_list(EndT)),
	[[{_,_},{Value, Num}]] = PHR,
	Rec ! {self(), {Value, Num}},
	{stop, normal, State};

handle_cast({get_posts, get_browser, Rec}, State) ->
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_browser?group=true"),
	Result = [{OS, Browser} || [{_, OS},{_, Browser}] <- R],
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_posts, get_language, Rec}, State) -> 
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_language?group=true"),
	Result = [{Language, Value} || [{_, Language},{_, Value}] <- R],
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_posts, get_platform, Rec}, State) ->
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_platform?group=true"),
	Result = [{Language, Value} || [{_, Language},{_, Value}] <- R],
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_posts, get_browser_version, Rec}, State) ->
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_browser_version?group=true"),
	Result = [{Browser, Version, Value} || [{_, [Browser, Version]},{_, Value}] <- R],
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_posts, get_platform_browser, Rec}, State) ->
	R = couch_operations:doc_get_mapreduce_cont(?DB ++ "_design/stat/_view/by_platform_browser?group=true"),
	Result = [{Platform, Browser, Value} || [{_, [Platform, Browser]},{_, Value}] <- R],
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_posts, {get_popular_within_time, StartT, EndT, Limit}, Rec}, State) ->
	PHR = couch_operations:doc_get_mapreduce__cont(
		   ?DB ++ "_design/stat/_view/within_time?startkey=" ++ integer_to_list(StartT) ++ "&endkay=" ++ integer_to_list(EndT)),
	Result = db_filter:limit_result(Limit, db_filter:order_by_value(PHR)),
	Rec ! {self(), Result},
	{stop, normal, State}.

%% @doc Handels the info (not used)
handle_info(_Info, _State) ->
	{noreply, _State}.

%% @doc Terminates the server
terminate(_Reason, _State) ->
	ok.

