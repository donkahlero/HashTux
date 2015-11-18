%% @author Jonas Kahler <jonas.kahler@icloud.com> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc A worker that handles the reading from the db for the userstats
%% @version 0.1
%% -----------------------------------------------------------------------------
%% | Sprint 4                                                                  |
%% | Version 0.1                                                               |
%% | This gen_server will work as a worker and handle the different messages   |
%% | from the sup for collecting  user statistics. Basic functionality is done,|
%% | it can read from the db and use mapreduce functions for getting the corre-|
%% | ct data.                                                                  |
%% -----------------------------------------------------------------------------
-module(db_userstats_reader).
-version(0.1).

-behavior(gen_server).

-export([start_link/0, stop/0, state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3, terminate/2]).

%% Local database params
-define(ADDR, fun() -> {ok, {ADDR, _, _}} =
              application:get_env(db_conf, localdb), ADDR end).
-define(USER, fun() -> {ok, {_, USER, _}} =
              application:get_env(db_conf, localdb), USER end).
-define(PASS, fun() -> {ok, {_, _, PASS}} =
              application:get_env(db_conf, localdb), PASS end).

%% -----------------------------------------------------------------------------
%% | Public API                                                                |
%% -----------------------------------------------------------------------------
%% @doc Starts the gen_server and links it to the calling process.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Stops the gen_server.
stop(Module) ->
    gen_server:call(Module, stop).
stop() ->
    stop(self()).

%% @doc Returns the state of the gen_sever
state(Module) ->
    gen_server:call(Module, state).
state() ->
    state(self()).

%% -----------------------------------------------------------------------------
%% | Server implementation                                                     |
%% -----------------------------------------------------------------------------
%% @doc Init function which starts the server with an empty state.
init([]) ->
    {ok, []}.

%% @doc Stop call to the server.
handle_call(stop, _From, _State) ->
    {stop, normal, stopped, _State};
%% @doc Other calls are not supported.
handle_call(_, _, _) ->
    error(undef).

%% @doc Get statistics based on the search term from the server.
handle_cast({get_stats, get_search_term, Options, Rec}, State) ->
    R = couch_operations:doc_get_mapreduce_cont({?ADDR() ++
             "hashtux_userstats/_design/stat/_view/by_search_term?" ++
             db_options_handler:pre_search_opt(
             db_options_handler:order_options(Options)) ++ "&group=true",
             ?USER(), ?PASS()}),
    Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
    Opt = db_options_handler:order_options(Options),
    LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt,
                                      {Atom, Limit} == {limit, Limit}], Result),
    Rec ! {self(), LR},
    {stop, normal, State};

%% @doc Get statistics based on the browserfrom the server.
handle_cast({get_stats, get_browser, Options, Rec}, State) ->
    R = couch_operations:doc_get_mapreduce_cont({?ADDR ++
             "hashtux_userstats/_design/stat/_view/by_browser?" ++
             db_options_handler:pre_search_opt(
             db_options_handler:order_options(Options)) ++ "&group=true",
             ?USER(), ?PASS()}),
    Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
    Opt = db_options_handler:order_options(Options),
    LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt,
                                      {Atom, Limit} == {limit, Limit}], Result),
    Rec ! {self(), LR},
    {stop, normal, State};

%% @doc Get statistics based the language on from the server.
handle_cast({get_stats, get_language, Options, Rec}, State) ->
    R = couch_operations:doc_get_mapreduce_cont({?ADDR() ++
             "hashtux_userstats/_design/stat/_view/by_language?" ++
             db_options_handler:pre_search_opt(
             db_options_handler:order_options(Options)) ++ "&group=true",
             ?USER(), ?PASS()}),
    Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
    Opt = db_options_handler:order_options(Options),
    LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt,
                                      {Atom, Limit} == {limit, Limit}], Result),
    Rec ! {self(), Result},
    {stop, normal, State};

%% @doc Get statistics based on the platform from the server.
handle_cast({get_stats, get_platform, Options, Rec}, State) ->
    R = couch_operations:doc_get_mapreduce_cont({?ADDR() ++
             "hashtux_userstats/_design/stat/_view/by_platform?" ++
             db_options_handler:pre_search_opt(
             db_options_handler:order_options(Options)) ++ "&group=true",
             ?USER(), ?PASS()}),
    Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
    Opt = db_options_handler:order_options(Options),
    LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt,
                                      {Atom, Limit} == {limit, Limit}], Result),
    Rec ! {self(), Result},
    {stop, normal, State};

%% @doc Get statistics based on the browser version from the server.
handle_cast({get_stats, get_browser_version, Options, Rec}, State) ->
    R = couch_operations:doc_get_mapreduce_cont({?ADDR() ++
             "hashtux_userstats/_design/stat/_view/by_browser_version?" ++
             db_options_handler:pre_search_opt(
             db_options_handler:order_options(Options)) ++ "&group=true",
             ?USER(), ?PASS()}),
    Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
    Opt = db_options_handler:order_options(Options),
    LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt,
                                      {Atom, Limit} == {limit, Limit}], Result),
    Rec ! {self(), Result},
    {stop, normal, State};

%% @doc Get statistics based on the platform/brower from the server.
handle_cast({get_stats, get_platform_browser, Options, Rec}, State) ->
    R = couch_operations:doc_get_mapreduce_cont({?ADDR() ++
             "hashtux_userstats/_design/stat/_view/by_platform_browser?" ++
             db_options_handler:pre_search_opt(
             db_options_handler:order_options(Options)) ++ "&group=true",
             ?USER(), ?PASS()}),
    Result = db_filter:order_by_value(db_filter:group_by_subkey(R)),
    Opt = db_options_handler:order_options(Options),
    LR = db_options_handler:search_opt([{limit, Limit} || {Atom, Limit}  <- Opt,
                                      {Atom, Limit} == {limit, Limit}], Result),
    Rec ! {self(), Result},
    {stop, normal, State}.

%% @doc Normal messages to the server are not supported.
handle_info(_Info, _State) ->
    error(undef).

%% doc Code update
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Terminates the server
terminate(_Reason, _State) ->
    ok.
