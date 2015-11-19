%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Main OTP supervisor for the CouchDB database part of our application.
%% @version 0.3
%% -----------------------------------------------------------------------------
%% | Sprint 1 // v0.1                                                          |
%% | Supervisor starts three children: Hashtag reader and writer supervisor as |
%% | our db_serv gen_server worker, which will wait for requests to the db.    |
%% -----------------------------------------------------------------------------
%% | Sprint 2 // v0.2                                                          |
%% | Supervisor starts now sub supervisor for the statistic writer workers.    |
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v0.3                                                          |
%% | Added the cleanup worker to the supervisor.                               |
%% | Added supervisor for the DB Stats readers.                                |
%% | Added the replicator worker to the supervisor.                            |
%% -----------------------------------------------------------------------------
-module(db_sup).
-version(0.3).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1, shell_start/0]).

%% @doc Starts the supervisor an links it to the calling process.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Function to start the supervisor from the shell. It unlinks the
%% supervisor from the shell - therefore the shell does not crash when a child
%% goes down. Just for testing purposes!
shell_start() ->
    {ok, Pid} = start_link(),
    unlink(Pid).

%% @doc Init function which creades all child options and then starts the
%% the children.
init([]) ->
    HashReadSup = {db_hash_read_sup, {db_worker_sup, start_link,
                  [db_hash_read_sup]}, temporary, 5000, supervisor,
                  [db_worker_sup]},
    HashWriteSup = {db_hash_write_sup, {db_worker_sup, start_link,
                   [db_hash_write_sup]}, temporary, 5000, supervisor,
                   [db_worker_sup]},
    StatsWriteSup = {db_stats_write_sup, {db_worker_sup, start_link,
                    [db_stats_write_sup]}, temporary, 5000, supervisor,
                    [db_worker_sup]},
    StatsReadSup = {db_stats_read_sup, {db_worker_sup, start_link,
                   [db_stats_read_sup]}, temporary, 5000, supervisor,
                   [db_worker_sup]},
    DBCleaner = {db_cleaner, {db_cleaner, start_link, []},
                permanent, 5000, worker, [db_cleaner]},
    DBReplicator = {db_replicator, {db_replicator, start_link, []},
                   permanent, 5000, worker, [db_replicator]},
    DBServ = {db_serv, {db_serv, start_link, []},
             permanent, 5000, worker, [db_serv]},
    {ok, {{one_for_one, 1, 10}, [HashReadSup, HashWriteSup, StatsWriteSup,
         StatsReadSup, DBCleaner, DBReplicator, DBServ]}}.
