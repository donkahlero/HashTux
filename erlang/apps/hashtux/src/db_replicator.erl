%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Module taking care of replicating the database in a given time interval.
%% @version 0.1
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v0.1:                                                         |
%% | Added functions to                                                        |
%% | - Replicate the statistic database                                        |
%% -----------------------------------------------------------------------------
-module(db_replicator).
-version(0.1).

-export([start_link/0]).

%% Local Database params
-define(ADDR, fun() -> {ok, {ADDR, _, _}} =
              application:get_env(db_conf, localdb), ADDR end).
-define(USER, fun() -> {ok, {_, USER, _}} =
              application:get_env(db_conf, localdb), USER end).
-define(PASS, fun() -> {ok, {_, _, PASS}} =
              application:get_env(db_conf, localdb), PASS end).

%% @doc Starts the replication worker and links it to the calling process
start_link() ->
    {ok, spawn_link(fun() -> replicate() end)}.

%% @doc Function to replicate the database in a given time interval
replicate() ->
    {ok, DBList} = application:get_env(db_conf, external),
    replicate_userstats(DBList),
    %Replicate every 15 minutes
    timer:sleep(60000 * 15),
    replicate().

%% @doc Replaction function. Replicates the local database to a target database.
%% This works for a cluster up to ~5 nodes. For more a tweaked connector is
%% needed.
replicate_userstats([]) ->
    ok;
replicate_userstats([{Addr, User, Pass}|Xs]) ->
    couch_connector:post_request({?ADDR() ++ "_replicate", User, Pass},
                   "{\"source\":\"hashtux_userstats\",\"target\":\"" ++ Addr ++
                   "hashtux_userstats\"}", "application/json"),
    replicate_userstats(Xs).
