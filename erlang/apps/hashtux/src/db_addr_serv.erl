-module(db_addr_serv).

-behavior(gen_server).

-export([start_link/0, stop/0, state/0]).
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

%% -----------------------------------------------------------------------------
%% | Server implementation. Further documentation for each call. Casts, and    |
%% | get_info is not supported yet.                                            |
%% -----------------------------------------------------------------------------
%% @doc db_serv init function. No argument needed to be passed.
init([]) ->
    io:format("db_addrfetcher started...\n", []),
    [MainDB | ExternalDBs] = get_dbs(),
    init_designdocs(MainDB),
    {ok, [MainDB | ExternalDBs]}.

handle_call(get_maindb, _From, [MainDB | ExternalDBs]) ->
    {reply, MainDB, [MainDB | ExternalDBs]};
handle_call(get_externaldbs, _From, [MainDB | ExternalDBs]) ->
    {reply, ExternalDBs, [MainDB | ExternalDBs]}.

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
    Rec = self(),
    PID = spawn(fun() -> Rec ! couch_connector:get_info(DB) end),
    receive
        {ok,{{"HTTP/1.1",200,"OK"}, _, _}} ->
            true
    after 1000 ->
            exit(PID, normal),
            false
    end.

init_designdocs({Addr, User, Pass}) ->
    couch_operations:doc_change({Addr ++ "hashtux/_design/post",
                                 User, Pass}, db_designdocs:get_post()),
    couch_operations:doc_change({Addr ++ "hashtux_userstats/_design/stat",
                                 User, Pass}, db_designdocs:get_stat()).
