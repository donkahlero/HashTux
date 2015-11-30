%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Module removing and creating databases in a given time interval.
%% @version 0.1
%% -----------------------------------------------------------------------------
%% | Sprint 5 // v0.1:                                                         |
%% | Initial version of this module which backups the database first and then  |
%% | creates a new copy of it and puts the backup in that one. It does that on |
%% | a given time interval (recommented is 3h).                                |
%% -----------------------------------------------------------------------------
-module(db_eraser).
-version(0.1).

-export([start_link/0]).

%% @doc Starts the cleaner worker and links it to the calling process.
start_link() ->
    {ok, spawn_link(fun() -> eraser() end)}.

%% @doc Cleanup method which calls the acutal compact_db function in a given
%% time interval. Does the same with the delete entries fun.
eraser() ->
    %% Backup original db
    couch_connector:post_request({db_addr_serv:main_addr() ++ "_replicate",
                   db_addr_serv:main_user(), db_addr_serv:main_pass()},
                   "{\"source\":\"hashtux\",\"target\":\"hashtux_bu\",
                   \"create_target\":true}",
                   "application/json"),
    %% Erase the original db
    couch_operations:delete_db({db_addr_serv:main_addr() ++ "hashtux",
                     db_addr_serv:main_user(), db_addr_serv:main_pass()}),
    %% Create a new clean db out of the backup
    couch_connector:post_request({db_addr_serv:main_addr() ++ "_replicate",
                   db_addr_serv:main_user(), db_addr_serv:main_pass()},
                   "{\"source\":\"hashtux_bu\",\"target\":\"hashtux\",
                   \"create_target\":true}",
                   "application/json"),
    %% Erase the backup db
    couch_operations:delete_db({db_addr_serv:main_addr() ++ "hashtux_bu",
                     db_addr_serv:main_user(), db_addr_serv:main_pass()}),
    %% Every 3h
    timer:sleep(10800000),
    eraser().
