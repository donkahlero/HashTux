%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte <niklas.lecomte@hotmail.com>
%% @version 0.2
%% -----------------------------------------------------------------------------
%% | Sprint 3 // v0.1                                                          |
%% | Added this worker to write user statistics to the database.               |
%% | The following function are possible:                                      |
%% | - General GEN_Serv functionalities                                        |
%% | - Server writes data to hashtux_userstats db                              |
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v0.2                                                          |
%% | Cleaned up the code. Server calls now couch_operations as well as         |
%% | With the db addresses defined in the config file. Therefore the macros    |
%% | were added.                                                               |
%% -----------------------------------------------------------------------------
-module(db_userstats_writer).
-version(0.2).

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
%% | Public gen_server API                                                     |
%% -----------------------------------------------------------------------------
%% @doc Start the gen_server and link it to the calling process.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Stop the gen_server.
stop(Module) ->
    gen_server:call(Module, stop).
stop() ->
    stop(?MODULE).

%% @doc Get the current state of the gen_server
state(Module) ->
    gen_server:call(Module, state).
state() ->
    state(?MODULE).

%% -----------------------------------------------------------------------------
%% | Server implementation                                                     |
%% -----------------------------------------------------------------------------
%% @doc Server init function. Starts the server with an empty state.
init([]) ->
    {ok, []}.

%% @doc Call to stop the server.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
%% @doc Other calls are not supportet. Thorw an error.
handle_call(_, _, _) ->
    error(badarg).

%% @doc Write data to the userstats database.
handle_cast({add_doc, Content, Rec}, State) ->
    UUID = couch_operations:get_uuid(),
    couch_operations:doc_add({?ADDR ++ "hashtux_userstats/" ++ UUID,
                              ?USER, ?PASS()}, Content),
    Rec ! {self(), true},
    {stop, normal, State}.

%% @doc Direct messages to the server are not supported yet.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Code_change and update method.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Terminate function
terminate(_Reason, _State) ->
    ok.
