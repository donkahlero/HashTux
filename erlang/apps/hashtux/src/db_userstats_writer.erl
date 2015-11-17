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


handle_cast({add_doc, Content, Rec}, State) ->
	UUID = couch_operations:get_uuid(),
	couch_operations:doc_add(?DB ++ UUID, Content),
	Rec ! {self(), true},
	{stop, normal, State}.

handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.
