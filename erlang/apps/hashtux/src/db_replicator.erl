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

-export([start_link/0]).

start_link() ->
    {ok, spawn_link(fun() -> cleanup() end)}.

cleanup() ->
    replicate_userstats(),
    %Replicate every 15 minutes
    timer:sleep(60000 * 15),
    cleanup().

%% @doc Replaction function. Replicates the local database to a target database.
%% This works for a cluster up to ~5 nodes. For more a tweaked connector is
%% needed.
replicate_userstats() ->
    couch_connector:post_request("_replicate", 
	"{\"source\":\"hashtux_userstats\",\"target\":\"http://hashtux:grouptux@tikser.se:1984/hashtux_userstats\"}",
	"application/json").
