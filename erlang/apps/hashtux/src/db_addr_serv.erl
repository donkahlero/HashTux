%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @doc Module for fetching and evaluating database addresses. On init the
%% module tries to access a database. When this database does not respond in a
%% given amount of time the database is treated as unavailable and another
%% database is used as the main database for the node.
%% @version 0.1
%% -----------------------------------------------------------------------------
%% | Sprint 5 // v0.1                                                          |
%% | First version of this module. Does the things as described above.         |
%% -----------------------------------------------------------------------------
-module(db_addr_serv).

-behavior(gen_server).

-export([start_link/0, stop/0, state/0]).
-export([main_addr/0, main_user/0, main_pass/0, external_dbs/0]).
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% -----------------------------------------------------------------------------
%% | Public access functions. Contain a start_link function, a function to     |
%% | stop the server and get its current state.                                |
%% -----------------------------------------------------------------------------
%% @doc Start the gen_server and link it to the calling process.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the db_serv
stop() ->
  gen_server:cast(?MODULE, stop).

%% @doc Get the current db_serv state
state() ->
  gen_server:call(?MODULE, state).

%% @doc Get the address of the main server
main_addr() ->
    {Addr, _, _} = gen_server:call(?MODULE, get_maindb),
    Addr.

%% @doc Get the user of the main database
main_user() ->
    {_, User, _} = gen_server:call(?MODULE, get_maindb),
    User.

%% @doc Get the password of the main database
main_pass() ->
    {_, _, Pass} = gen_server:call(?MODULE, get_maindb),
    Pass.

%% @doc get the external databases available
external_dbs() ->
    gen_server:call(?MODULE, get_external_dbs).

%% -----------------------------------------------------------------------------
%% | Server implementation. Further documentation for each call. Casts, and    |
%% | get_info is not supported yet.                                            |
%% -----------------------------------------------------------------------------
%% @doc db_serv init function. No argument needed to be passed.
init([]) ->
    io:format("db_addr_server started...\n", []),
    [MainDB | ExternalDBs] = get_dbs(),
    init_designdocs(MainDB),
    {ok, {MainDB, ExternalDBs}}.

handle_call(get_maindb, _From, DBs) ->
    {MainDB, _} = DBs,
    {reply, MainDB, DBs};
handle_call(get_external_dbs, _From, DBs) ->
    {_, ExternalDBs} = DBs,
    {reply, ExternalDBs, DBs}.

%%% Other casts are not implemented yet.
handle_cast(_, _) ->
    error(undef).

%% @doc Handles all other messages to the server. These are not supported and
%% will let the server crash.
handle_info(_, _) ->
    error(undef).

%% @doc Termination function. Need for implementation.
terminate(_Reason, _State) ->
    ok.

%% @doc Implementation req.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------------------------
%% | Helperfunctions for evaluating the databases and updating/adding          |
%% | the necessary design documents.                                           |
%% -----------------------------------------------------------------------------
get_dbs() ->
    {ok, LocalDB} = application:get_env(db_conf, localdb),
    {ok, ExternalDB} = application:get_env(db_conf, external),
    [X || X <- [LocalDB | ExternalDB], eval_db(X) =:= true].

eval_db(DB) ->
    case (couch_connector:get_info(DB)) of
        {ok,{_, _, Body}} ->
            lists:keyfind(<<"couchdb">>, 1, jsx:decode(Body)) =:=
                                            {<<"couchdb">>, <<"Welcome">>};
        {error, _} ->
            false
    end.

init_designdocs({Addr, User, Pass}) ->
    couch_operations:doc_change({Addr ++ "hashtux/_design/post",
                                 User, Pass}, db_designdocs:get_post()),
    couch_operations:doc_change({Addr ++ "hashtux_userstats/_design/stat",
                                 User, Pass}, db_designdocs:get_stat()).
