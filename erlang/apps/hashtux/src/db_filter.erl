%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Module repsonsible for filtering database results. The functions always
%% take lists and necessary information as arguments and filter the list
%% corresponding to that.
%% @version 0.2
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v0.1:                                                         |
%% | Added initial version of this module.                                     |
%% | Module can handle the following filters:                                  |
%% | - content_type, language and service                                      |
%% -----------------------------------------------------------------------------
%% | Sprint % // v0.2:                                                         |
%% | Bugfixes and reducing of code in                                          |
%% | - check_results                                                           |
%% | - timeframe                                                               |
%% -----------------------------------------------------------------------------
-module(db_filter).

-export([content_type/2, language/2, service/2, order_by_value/1]).
-export([limit_result/2, group_by_subkey/1, check_results/2, timeframe/2]).

%% @doc Function checking if the miners cannot find something or there is just
%% nothing cached yet.
check_results([], _) ->
    [];
check_results([[{<<"results">>, <<"no">>}, {<<"search_term">>, _},
                {<<"timestamp">>, _}, {<<"options">>, Opts}] | _], Opts) ->
    no_miner_res;
check_results([_|Xs], Opts) ->
    check_results(Xs, Opts).

%% @doc Function filtering for the type of content.
%% This can be image, video or text.
%% Several types within one search are possible.
content_type(L, false) ->
    L;
content_type(L, {content_type, CTypes}) ->
    [X || X <- L, Y <- CTypes,
          lists:keyfind(<<"content_type">>, 1, X) == {<<"content_type">>, Y}].

%% @doc Function filtering for the language of the content.
%% Just one language is possible here.
language(L, false) ->
    L;
language(L, {language, Language}) ->
    [X || X <- L,
          lists:keyfind(<<"language">>, 1, X) == {<<"language">>, Language}].

%% @doc Function for filtering the content for the service.
%% One or more services are possible: twitter, instagram and
%% youtube.
service(L, false) ->
    L;
service(L, {service, Services}) ->
    [X || X <- L, Y <- Services,
    lists:keyfind(<<"service">>, 1, X) == {<<"service">>, Y}].

%% @doc Function to check if the elemnts in the doc are in a given timeframe.
timeframe(L, false) ->
    L;
timeframe(L, {timeframe, StartTime, EndTime}) ->
    [X || X <- L, {<<"timestamp">>, T} <-
       [lists:keyfind(<<"timestamp">>, 1, X)],
       StartTime =< T, T =< EndTime].

%% @doc Function ordering mapreduce results by their value
order_by_value(L) ->
    lists:reverse(lists:keysort(2,
      [{Key, Value} || [{<<"key">>, Key}, {<<"value">>, Value}] <- L])).

%% @doc Limits a list to be as long as the Num says
limit_result(L, false) ->
    L;
limit_result(L, {limit, Num}) when length(L) > Num->
    {R, _} = lists:split(Num, L),
    R;
limit_result(L, _) ->
    L.

%% @doc Function which orders the list by the value and not by the key.
group_by_subkey(L) ->
    KVs = [{Key, Value} ||
          [{<<"key">>, [_Timestamp, Key]}, {<<"value">>, Value}] <- L],
    [[{binary:list_to_bin("key"), Key}, {binary:list_to_bin("value"), Value}] ||
    {Key, Value} <- sum_values(lists:keysort(1, KVs), [])].

%% @doc Function which sums the values in a key value list.
sum_values([], Res) ->
    Res;
sum_values([{X, N} | Xs], [{X, M} | Ys]) ->
    sum_values(Xs, [{X, N+M} | Ys]);
sum_values([{X, N} | Xs], Y) ->
    sum_values(Xs, [{X, N} | Y]).
