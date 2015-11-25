%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Module taking care of cleaning up the database in a given time interval.
%% @version 0.3
%% -----------------------------------------------------------------------------
%% | Sprint 3 // v0.1:                                                         |
%% | Added functions to                                                        |
%% | - Compact the databases                                                   |
%% | - Remove entries which are older than 1 minute                            |
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v0.2:                                                         |
%% | Small clean up                                                            |
%% | Reads the database information out of the config file                     |
%% -----------------------------------------------------------------------------
%% | Sprint 5 // v0.3:                                                         |
%% | Fixed the cleanup function: will not crash when there are now results     |
%% | to delete.                                                                |
%% -----------------------------------------------------------------------------
-module(db_cleaner).
-version(0.3).

-export([start_link/0]).

%% @doc Starts the cleaner worker and links it to the calling process.
start_link() ->
    {ok, spawn_link(fun() -> cleanup() end)}.

%% @doc Cleanup method which calls the acutal compact_db function in a given
%% time interval. Does the same with the delete entries fun.
cleanup() ->
    delete_entries(),
    compact_db(),
    %Cleanup every minute
    timer:sleep(60000),
    cleanup().

%% @doc Function which deletes all entries in the database which are older than
%% a given amount of time.
delete_entries() ->
    Time = integer_to_list(calendar:datetime_to_gregorian_seconds(
       calendar:now_to_universal_time(os:timestamp()))-719528*24*3600-3600),
    Results = couch_operations:doc_get({db_addr_serv:main_addr() ++
       "hashtux/_design/post/_view/by_insert_timestamp?endkey=" ++
       Time, db_addr_serv:main_user(), db_addr_serv:main_pass()}),
    case(lists:keyfind(<<"rows">>, 1, Results)) of
        {_, Posts} ->
            [couch_operations:doc_delete({db_addr_serv:main_addr() ++
                "hashtux/" ++ binary_to_list(ID), db_addr_serv:main_user(),
                db_addr_serv:main_pass()}, binary_to_list(Rev)) ||
                [{<<"_id">>, ID}, {<<"_rev">>, Rev} | _] <- [Content ||
                {<<"value">>, Content} <- lists:flatten(Posts)]];
        false ->
            no_results
        end,
    ok.

%% @doc Function which compacts the database.
compact_db() ->
    couch_connector:post_request({db_addr_serv:main_addr() ++
                    "hashtux/_compact", db_addr_serv:main_user(),
                    db_addr_serv:main_pass()}, [], "application/json"),
    couch_connector:post_request({db_addr_serv:main_addr() ++
                    "hashtux/_view_cleanup", db_addr_serv:main_user(),
                    db_addr_serv:main_pass()}, [], "application/json"),
    couch_connector:post_request({db_addr_serv:main_addr() ++
                    "hashtux_userstats/_compact", db_addr_serv:main_user(),
                    db_addr_serv:main_pass()}, [], "application/json"),
    couch_connector:post_request({db_addr_serv:main_addr() ++
                    "hashtux_userstats/_view_cleanup", db_addr_serv:main_user(),
                    db_addr_serv:main_pass()}, [], "application/json").
