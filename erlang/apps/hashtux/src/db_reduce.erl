%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @doc Module which reduces data coming from large lists coming from the
%% userstats database. Spawns subworkers to deal with smaller chunks of data.
%% @version 0.1
%% -----------------------------------------------------------------------------
%% | Sprint 5 // v0.1                                                          |
%% | Initial version of this module.                                           |
%% | Main function is reduce. This can be called with a normal list (no rev    |
%% | or ids or with these fields containing.                                   |
%% | This list is then split up into sub list (for each day). Foreach of these |
%% | sublist a worker is spawned which is sorting them (rereducing) the reduce |
%% | reduces then these lists to one together.                                 |
%% -----------------------------------------------------------------------------
-module(db_reduce).

-export([reduce/1]).

%% @doc General reduce function. See main doc.
reduce([_, _, {_, L}]) ->
    reduce(L);
reduce(L) ->
    {<<"key">>, StartTime} = lists:keyfind(<<"key">>, 1, lists:nth(1, L)),
    {<<"key">>, EndTime} = lists:keyfind(<<"key">>, 1, lists:last(L)),
    PIDs = spawn_workers(splitup(L, gen_tf(StartTime, EndTime), []), []),
    ResList = receive_res(PIDs, []),
    lists:usort([{gen_key(T), Amount} ||
      [{<<"key">>, T}, {<<"value">>, _}] <- ResList,
      Amount <- [lists:sum([Counter || [{<<"key">>, T2},
      {<<"value">>, Counter}] <- ResList, T2 =:= T])]]).

%% @doc Creates a timeframe based on the oldest (StartTime) and newest
%% (EndTime) in the document passed to the reduce.
gen_tf(StartTime, EndTime) when (EndTime - StartTime) < 86400 ->
    [EndTime];
gen_tf(StartTime, EndTime) ->
    lists:usort(lists:append(lists:seq(StartTime, EndTime, 86400), [EndTime])).

%% @doc Splits up the list according to the time frame into chunks foreach day.
splitup([], _, SplitRes) ->
    SplitRes;
splitup(L, [Time|Times], SplitRes) ->
    F = fun([{_, _}, {_, CurrTime}, {_, _}]) ->
          eval_time(CurrTime, Time) end,
    {L1, L2} = lists:partition(F, L),
    splitup(L2, Times, [L1|SplitRes]).

%% @doc Generate the key (if it consists out of seperate keys).
gen_key([K1, K2]) ->
    binary:list_to_bin(binary_to_list(K1) ++ ", " ++ binary_to_list(K2));
gen_key(K) ->
    K.

%% @doc Helperfunction evaluating if a Time is within hat timeframe.
eval_time(Time, CompareTime) when Time =< CompareTime ->
    true;
eval_time(_, _) ->
    false.

%% @doc Helperfunction for receiving the results from the spawned workers /pmap.
receive_res([], ResList) ->
    ResList;
receive_res([X|Xs], ResList) ->
    receive
        {X, Res} ->
            receive_res(Xs, lists:append(ResList, Res))
    end.

%% @doc Function that spawns workers and returns a list of their PIDs.
spawn_workers([], PIDs) ->
    PIDs;
spawn_workers([X|Xs], PIDs) ->
    S = self(),
    PID = spawn(fun() -> S ! {self(), rereduce(X)} end),
    spawn_workers(Xs, [PID| PIDs]).

%% @doc Rereduce function which reduces the sublists.
rereduce(L) ->
    WL = [[{<<"key">>, Term}, {<<"value">>, 1}] ||
      [{_, _}, {<<"key">>, _}, {<<"value">>, Term}] <- L],
    [[{<<"key">>, T}, {<<"value">>, Amount}] ||
      [{<<"key">>, T}, {_, _}] <- lists:usort(WL),
      Amount <- [length([Counter || [{<<"key">>, T2},
      {<<"value">>, Counter}] <- WL, T2 =:= T])]].
