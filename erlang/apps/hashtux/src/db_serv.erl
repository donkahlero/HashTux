%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Dispenser Server for all calls to the database. Outsources
%% all work to worker processes which will be added to the responsible
%% supervisor. Is part of the db_sup tree.
%% @version 0.1
%% -----------------------------------------------------------------------------
%% | Sprint 1 // v0.1:                                                         |
%% | Initial version. Is able to start workers with unique id an append        |
%% | them to the responsible supervisor. Is able to handle simple calls - more |
%% | will be added in later sprints/interations.                               |
%% -----------------------------------------------------------------------------
-module(db_serv).

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
    io:format("db_serv started...", []),
    {ok, []}.

%%  @doc handles all kinds of calls to the server. Not supported calls will
%%  lead in an error.
%%% Get the complete content of a hashtag document from the database.
handle_call({get_hash, Hashtag}, {From, _Ref}, State) ->
    {ok, Ref} = start_rw(),
    gen_server:cast(Ref, {get_hash, Hashtag, From}),
    {reply, Ref, State};
%%% Get the content of a hashtag document without the id an rev.
handle_call({get_cont, Hashtag}, {From, _Ref}, State) ->
    {ok, Ref} = start_rw(),
    gen_server:cast(Ref, {get_cont, Hashtag, From}),
    {reply, Ref, State};
%%% Check if a document for a hashtag exists.
handle_call({hash_exist, Hashtag}, {From, _Ref}, State) ->
    {ok, Ref} = start_rw(),
    gen_server:cast(Ref, {hash_exist, Hashtag, From}),
    {reply, Ref, State};
%%% Add a hashtag document to the database.
handle_call({add_hash, Hashtag, Content}, {From, _Ref}, State) ->
    {ok, Ref} = start_ww(),
    gen_server:cast(Ref, {add_hash, Hashtag, Content, From}),
    {reply, Ref, State};
%%% Overwrite a hashtag document
handle_call({overwr_hash, Hashtag, Content}, {From, _Ref}, State) ->
    {ok, Ref} = start_ww(),
    gen_server:cast(Ref, {overwr_hash, Hashtag, Content, From}),
    {reply, Ref, State};
%%% Add content to a hashtag document
handle_call({add_content, Hashtag, Content}, {From, _Ref}, State) ->
    {ok, Ref} = start_ww(),
    gen_server:cast(Ref, {add_cont, Hashtag, Content, From}),
    {reply, Ref, State};
%%% Remove an entry from a hashtag document
handle_call({remove_val, Hashtag, Field}, {From, _Ref}, State) ->
    {ok, Ref} = start_ww(),
    gen_server:cast(Ref, {remove_val, Hashtag, Field, From}),
    {reply, Ref, State};
%%% Delete a hashtag document in the database.
handle_call({delete_hash, Hashtag}, {From, _Ref}, State) ->
    {ok, Ref} = start_ww(),
    gen_server:cast(Ref, {delete_hash, Hashtag, From}),
    {reply, Ref, State};
%%% Get the current server state.
handle_call(state, _From, State) ->
    {reply, State, State};
%%% Other calls are not implemented yet.
handle_call(_, _, _) ->
    error(undef).

%% @doc Handles the casts to the server. Just a stop cast is supported. All
%% other casts will let the server crash.
%%% Cast handling to stop the server.
handle_cast(stop, State) ->
    io:format("db_serv stopped...", []),
    {stop, normal, State};
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
%% | Part which handles worker starts. The methods will create unique IDs as   |
%% | well as appending them to the supervisors.                                |
%% -----------------------------------------------------------------------------
%% @doc Function which starts a writer worker.
%% This type of worker performs WRITE ONLY stuff.
start_ww() ->
    ChildSpecs = {erlang:unique_integer(), {db_hash_writer, start_link, []},
		  temporary, 5000, worker, [db_hash_writer]},
    supervisor:start_child(db_hash_write_sup, ChildSpecs).

%% @doc Function which starts a reader worker.
%% This reader worker gets appended to the reader 
%% supervisor. 
start_rw() ->
    ChildSpecs = {erlang:unique_integer(), {db_hash_reader, start_link, []},
                  temporary, 5000, worker, [db_hash_reader]},
    supervisor:start_child(db_hash_read_sup, ChildSpecs).
