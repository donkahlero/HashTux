-module(db_userstats_writer).

-behaviour(gen_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DB, "hashtux_userstats/").

%% Public API

start_link() ->
	gen_server:start_link(?MODULE, [], []).

stop(Module) ->
	gen_server:call(Module, stop).

stop() ->
	stop(?MODULE).

state(Module) ->
	gen_server:call(Module, state).

state() ->
	state(?MODULE).



init([]) ->
	{ok, []}.

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(_, _, _) ->
	error(badarg).


handle_cast({add_session, Session, Content, Rec}, State) ->
	Date = binary:list_to_bin(integer_to_list(calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600)),
	couch_operations:doc_add(Session, [{<<"timestamp">>, Date}] ++ Content, ?DB),
	Rec ! {self(), true},
	{stop, normal, State};

handle_cast({add_content, Session, Content, Rec}, State) ->
	Date = binary:list_to_bin(integer_to_list(calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600)),
	couch_operations:doc_append(Session, [{<<"timestamp">>, Date}] ++ Content, ?DB),
	{stop, normal, State}.

handle_cast()

handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.
