%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Dispenser Server for all calls to the database. Outsources
%% all work to worker processes which will be added to the responsible
%% supervisor. Is part of the db_sup tree.
%% @version 0.2
%% -----------------------------------------------------------------------------
%% | Sprint 1 // v0.1:                                                         |
%% | Initial version. Is able to start workers with unique id an append        |
%% | them to the responsible supervisor. Is able to handle simple calls - more |
%% | will be added in later sprints/interations.                               |
%% -----------------------------------------------------------------------------
%% | Sprint 2 // v0.2:                                                         |
%% | Several name changes to more descriptive worker names.                    |
%% | Added support for userstats workers.                                      |
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
handle_call({get_posts, Hashtag}, {From, _Ref}, State) ->
    {ok, Ref} = start_hrw(),
    gen_server:cast(Ref, {get_posts, Hashtag, From}),
    {reply, Ref, State};
%%% Get content with some options
handle_call({get_posts, Hashtag, Options}, {From, _Ref}, State) ->
    {ok, Ref} = start_hrw(),
    gen_server:cast(Ref, {get_posts, Hashtag, Options, From}),
    {reply, Ref, State};
%%% Check if a document for a hashtag exists.
handle_call({posts_exist, Hashtag}, {From, _Ref}, State) ->
    {ok, Ref} = start_hrw(),
    gen_server:cast(Ref, {posts_exist, Hashtag, From}),
    {reply, Ref, State};
%%% Add hashtag documents to the database.
handle_call({add_doc, Content}, {From, _Ref}, State) ->
    {ok, Ref} = start_hww(),
    gen_server:cast(Ref, {add_doc, Content, From}),
    {reply, Ref, State};
%%% Add userstats to the database.
handle_call({add_habit_doc, Content}, {From, _Ref}, State) ->
    {ok, Ref} = start_usww(),
    gen_server:cast(Ref, {add_doc, Content, From}), 
    {reply, Ref, State};
%%% Delete a hashtag document in the database.
handle_call({delete_hash, Hashtag}, {From, _Ref}, State) ->
    {ok, Ref} = start_hww(),
    gen_server:cast(Ref, {delete_hash, Hashtag, From}),
    {reply, Ref, State};
%%% Getting the whole list of requested hashtags
handle_call({get_hash_count}, {From, _Ref}, State) ->
    {ok, Ref} = start_usrw(),
    gen_server:cast(Ref, {get_hash_count, From}),
    {reply, Ref, State};
%%% Getting and ordered list of requested hashtags
handle_call({get_popular_hash, Num}, {From, _Ref}, State) ->
    {ok, Ref} = start_usrw(),
    gen_server:cast(Ref, {get_popular_hash, Num, From}),
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
%% @doc Function which starts a hashtag writer worker.
%% This type of worker performs WRITE ONLY stuff.
start_hww() ->
    ChildSpecs = {erlang:unique_integer(), {db_hash_writer, start_link, []},
		  temporary, 5000, worker, [db_hash_writer]},
    supervisor:start_child(db_hash_write_sup, ChildSpecs).

%% @doc Function which starts a hashtag reader worker.
%% This reader worker gets appended to the ht reader 
%% supervisor. 
start_hrw() ->
    ChildSpecs = {erlang:unique_integer(), {db_hash_reader, start_link, []},
                  temporary, 5000, worker, [db_hash_reader]},
    supervisor:start_child(db_hash_read_sup, ChildSpecs).

%% @doc Function which starts a userstats writer worker.
start_usww() ->
    ChildSpecs = {erlang:unique_integer(), {db_userstats_writer, start_link,
					    []},
                  temporary, 5000, worker, [db_userstats_writer]},
    supervisor:start_child(db_stats_write_sup, ChildSpecs).

start_usrw() ->
    ChildSpecs = {erlang:unique_integer(), {db_userstats_reader, start_link,
                                            []},
                  temporary, 5000, worker, [db_userstats_reader]},
    supervisor:start_child(db_stats_read_sup, ChildSpecs).
