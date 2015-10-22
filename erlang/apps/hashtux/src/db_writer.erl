
%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% %% @doc Initial database actions module
%% %% %% @version 0.1

-module(db_writer).

-behavior(gen_server).

-export([start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


%% Public API

%% @doc Starts the server
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% @doc Stops the server
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

%% @doc Catches all cases except stop
handle_call(_, _, _) ->
	error(badarth).

%% %% @doc These handels the messages coming in and do the operations

handle_cast({add_hash, Hash, Content, Rec}, State) ->
	couch_operations:doc_add(Hash, Content),
        Rec ! true,	
	{stop, normal, State};

handle_cast({overwr_hash, Hash, Content, Rec}, State) ->
	couch_operations:doc_change(Hash, Content),
	Rec ! true,
	{stop, normal, State};

handle_cast({add_cont, Hash, Content, Rec}, State) ->
	couch_operations:doc_append(Hash, Content),
	Rec ! true,
	{stop, normal, State};

handle_cast({remove_val, Hash, Field, Rec}, State) ->
	couch_operations:doc_rmval(Hash, Field),
	Rec ! true,
	{stop, normal, State};

handle_cast({delete_hash, Hash, Rec}, State) ->
	couch_operations:doc_delete(Hash),
	Rec ! true,
	{stop, normal, State}.

%% @doc Handles Info (not used) 
handle_info(_Info, _State) ->
	{noreply, _State}.

%% @doc Terminates the server
terminate(_Reason, _State) -> 
  	ok.

