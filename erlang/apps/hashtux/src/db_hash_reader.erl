%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% @doc Initial database actions module
%% %% @version 0.1

%% -----------------------------------------------------------------------------
%% | Sprint 1 // v0.1                                                          |
%% | Created first as a normal module where the diffrent write operations was  |
%% | functions. Then made it into a gen_server which handled a lot of calls.   |
%% | It used every write operation defind in couch_operation.                  |
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% | Sprint 2 // v0.2                                                          |
%% | Changed the module to only do two different operations, get_posts and     |
%% | hash_exists to fit the new changes on how we will store data and what to  |
%% | get from the database.                                                    |
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v0.3                                                          |
%% | Added options to the advance search that will handle the different ones   |
%% | that is sent. It fetch all the result from the DB and the filters it      |
%% -----------------------------------------------------------------------------
-module(db_hash_reader).

-behaviour(gen_server).

-export([start_link/0, stop/0, state/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([search_opt/2, order_options/1]).

-define(DB, "hashtux/").

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

handle_cast({get_posts, Hashtag, Rec}, State) ->
    Result =  couch_operations:doc_get_map_cont(
		?DB ++ "_design/post/_view/by_hashtag?key=\"" ++ Hashtag ++  "\""), 
    Rec ! {self(), Result},
    {stop, normal, State};

handle_cast({get_posts, Hashtag, Options, Rec}, State) ->
	Hash_Result = couch_operations:doc_get_map_cont(
			?DB ++ "_design/post/_view/by_hashtag?key=\"" ++ Hashtag ++ "\""),
	Result = search_opt(order_options(Options), Hash_Result),
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({posts_exist, Hashtag, Rec}, State) ->
    Result = couch_operations:doc_exist(
	       ?DB ++ "_design/post/_view/by_hashtag?key=\"" ++ Hashtag ++ "\""),
    Rec ! {self(), Result},
    {stop, normal, State}.

%% @doc Handels the info (not used)
handle_info(_Info, _State) ->
    {noreply, _State}.

%% @doc Terminates the server
terminate(_Reason, _State) ->
    ok.


%% @doc This orders the list of options so that the limit is last.
order_options(Opt) ->
	order_options(Opt, []).
order_options([], L) ->
	L;
order_options([{limit, Lim}| T], L) ->
	R = L ++ [{limit, Lim}],
	order_options(T, R);
order_options([H| T], L) -> 
	R = [H] ++ L,
	order_options(T, R).

%% @doc This returns a new list of json objects from the differnt options.
search_opt([], L) ->
	L;
search_opt([{content_type, CTs}| T], L) ->
	R = db_filter:content_type(L, CTs),
	search_opt(T, R);
search_opt([{service, Services}| T], L) ->
	R = db_filter:services(L, Services),
	search_opt(T, R);
search_opt([{language, Langs}| T], L) ->
	R = db_filter:language(L, Langs),
	search_opt(T, R);
search_opt([{limit, Num}| _], L) ->
	R = db_filter:limit_result(Num, L),
	R.

