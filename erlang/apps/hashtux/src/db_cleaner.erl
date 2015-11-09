%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Module taking care of cleaning up the database in a given time interval.
%% @version 0.1
%% -----------------------------------------------------------------------------
%% | Sprint 3 // v0.1:                                                         |
%% | Added functions to                                                        |
%% | - Compact the databases                                                   |
%% | - Remove entries which are older than 1 minute                            |
%% -----------------------------------------------------------------------------

-module(db_cleaner).

-export([cleanup/0, delete_entries/0]).

-define(DB, "hashtux/").

cleanup() ->
    delete_entries(),
    compact_db(),
    %Cleanup every minute
    timer:sleep(60000),
    cleanup().

delete_entries() ->
    Time = integer_to_list(calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(erlang:timestamp()))-719528*24*3600-60),
    Results = couch_operations:doc_get(?DB ++ "_design/post/_view/by_insert_timestamp?endkey=" ++  Time),
    {_, Posts} = lists:keyfind(<<"rows">>, 1, Results),
    [couch_operations:doc_delete(?DB ++ binary_to_list(ID), binary_to_list(Rev)) ||
	[{<<"_id">>, ID}, {<<"_rev">>, Rev} | _] <- [Content || {<<"value">>, Content} <- lists:flatten(Posts)]],
    ok.

compact_db() ->
    couch_connector:post_request("hashtux/_compact", [], "application/json"),
    couch_connector:post_request("hashtux_userstats/_compact", [], "application/json").
