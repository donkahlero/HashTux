
%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% %% @doc Initial database actions module
%% %% %% @version 0.1

%% -----------------------------------------------------------------------------
%% | Sprint 1 // v0.1                                                          |
%% | Created first as a normal module where the diffrent write operations was  |
%% | functions. Then made it into a gen_server which handled a lot of calls.   |
%% | It used every write operation defind in couch_operation.                  |
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% | Sprint 2 // v0.2                                                          |
%% | Changed how many operations to include because of changes in how data is  |
%% | stored in the database. Only keept the operations which was needed for now|
%% | add_doc, and delete_hash.                                                 |
%% -----------------------------------------------------------------------------

-module(db_hash_writer).

-behavior(gen_server).

-export([start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DB, "hashtux/").

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

handle_cast({add_doc, Content, Rec}, State) ->
        add_docs(Content),
	Rec ! {self(), true},	
	{stop, normal, State};

%% Rewrite this later!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
handle_cast({delete_hash, Hashtag, Rec}, State) ->
	Results = couch_operations:doc_get(?DB ++ "_design/post/_view/by_hashtag?key=\"" ++ Hashtag ++ "\""),
	{_, Posts} = lists:keyfind(<<"rows">>, 1, Results),
	[couch_operations:doc_delete(?DB ++ binary_to_list(ID)) || {<<"_id">>, ID} <- lists:flatten([Content || {<<"value">>, Content} <- lists:flatten(Posts)])],
        Rec ! {self(), true},
	{stop, normal, State}.

%% @doc Handles Info (not used) 
handle_info(_Info, _State) ->
	{noreply, _State}.

%% @doc Terminates the server
terminate(_Reason, _State) -> 
  	ok.

add_docs([]) -> ok;
add_docs([H|T]) ->
	UUID = couch_operations:get_uuid(),
	couch_operations:doc_add([?DB | [UUID]], H),
	add_docs(T).
