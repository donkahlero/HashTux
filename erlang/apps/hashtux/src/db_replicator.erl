%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Module taking care of replicating the database in a given time interval.
%% @version 0.2
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v0.1:                                                         |
%% | Added functions to                                                        |
%% | - Replicate the statistic database                                        |
%% -----------------------------------------------------------------------------
%% | Sprint 5 // v0.2:                                                         |
%% | Fixed the module. Is checking now for errors in the connections.          |
%% | Applied changes of db_addr_serv.                                          |
%% -----------------------------------------------------------------------------
-module(db_replicator).
-version(0.2).

-export([start_link/0]).

%% @doc Starts the replication worker and links it to the calling process
start_link() ->
    {ok, spawn_link(fun() -> replicate() end)}.

%% @doc Function to replicate the database in a given time interval
replicate() ->
    replicate_userstats(db_addr_serv:external_dbs()),
    %Replicate every 15 minutes
    timer:sleep(60000 * 15),
    replicate().

%% @doc Replaction function. Replicates the local database to a target database.
%% This works for a cluster up to ~5 nodes. For more a tweaked connector is
%% needed.
replicate_userstats([]) ->
    ok;
replicate_userstats([{Addr, User, Pass}|Xs]) ->
    couch_connector:post_request({db_addr_serv:main_addr() ++ "_replicate",
                   User, Pass},
                   "{\"source\":\"hashtux_userstats\",\"target\":\"" ++ Addr ++
                   "hashtux_userstats\"}", "application/json"),
    replicate_userstats(Xs).
