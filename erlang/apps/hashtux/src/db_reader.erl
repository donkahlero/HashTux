%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% @doc Initial database actions module
%% %% @version 0.1


-module(db_reader).

-behaviour(gen_server).

-export([start_link/0, stop/0, state/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Public API

start_link() ->
	gen_server:start_link(?MODULE, [], []).

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


handle_call(stop, _From, _State) ->
	{stop, normal, stopped, _State};

handle_call(_, _, _) ->
	error(badarth).

handle_cast({get_hash, Hash, Rec}, State) ->
	Result = jsx:decode(couch_operations:doc_get(Hash)),
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({get_cont, Hash, Rec}, State) ->
	Result =  [{Field, Val} || {Field, Val} <- jsx:decode(couch_operations:doc_get(Hash)), Field =/= <<"_id">>, Field =/= <<"_rev">>],
	Rec ! {self(), Result},
	{stop, normal, State};

handle_cast({hash_exist, Hash, Rec}, State) ->
	Result = couch_operations:doc_exist(Hash),
	Rec ! {self(), Result},
	{stop, normal, State}.

handle_info(_Info, _State) ->
	{noreply, _State}.

terminate(_Reason, _State) ->
	ok.




