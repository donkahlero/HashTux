%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @doc Module which reduces data coming from large lists coming from the
%% userstats database. Spawns subworkers to deal with smaller chunks of data.
%% @version 0.1
%% -----------------------------------------------------------------------------
%% | Sprint 5 // v0.1                                                          |
%% | Initial draft of this module. Added a lot of subfunctions which are       |
%% | private.                                                                  |
%% -----------------------------------------------------------------------------
-module(db_reduce).

-export([reduce/1, gen_tf/2, splitup/3]).

%% @doc General reduce function
reduce([_, _, {_, L}]) ->
    reduce(L);
reduce(L) ->
    {<<"key">>, EndTime} = lists:keyfind(<<"key">>, 1, lists:nth(1, L)),
    {<<"key">>, StartTime} = lists:keyfind(<<"key">>, 1, lists:last(L)),
    PIDs = spawn_workers(splitup(L, gen_tf(StartTime, EndTime), []), []),
    ResList = receive_res(PIDs, []),
    lists:usort([[{<<"key">>, T}, {<<"value">>, Amount}] ||
      [{<<"key">>, T}, {<<"value">>, _}] <- ResList,
      Amount <- [lists:sum([Counter || [{<<"key">>, T2},
      {<<"value">>, Counter}] <- ResList, T2 =:= T])]]).

receive_res([], ResList) ->
    ResList;
receive_res([X|Xs], ResList) ->
    receive
        {X, Res} ->
            receive_res(Xs, lists:append(ResList, Res))
    end.

spawn_workers([], PIDs) ->
    PIDs;
spawn_workers([X|Xs], PIDs) ->
    S = self(),
    PID = spawn(fun() -> S ! {self(), rereduce(X)} end),
    spawn_workers(Xs, [PID| PIDs]).

rereduce(L) ->
    WL = [[{<<"key">>, Term}, {<<"value">>, 1}] ||
      [{_, _}, {<<"key">>, _}, {<<"value">>, Term}] <- L],
    [[{<<"key">>, T}, {<<"value">>, Amount}] ||
      [{<<"key">>, T}, {_, _}] <- lists:usort(WL),
      Amount <- [length([Counter || [{<<"key">>, T2},
      {<<"value">>, Counter}] <- WL, T2 =:= T])]].

gen_tf(StartTime, EndTime) when (EndTime - StartTime) < 86400 ->
    [StartTime, EndTime];
gen_tf(StartTime, EndTime) ->
    TF = lists:seq(StartTime, EndTime, 86400),
    case (lists:last(TF)) of
        EndTime ->
            TF;
        _ ->
            lists:append(TF, [EndTime])
    end.

splitup([], _, SplitRes) ->
    SplitRes;
splitup(L, [Time|Times], SplitRes) ->
    F = fun([{_, _}, {_, CurrTime}, {_, _}]) ->
          eval_time(CurrTime, Time) end,
    {L1, L2} = lists:partition(F, L),
    splitup(L2, Times, [L1|SplitRes]).

eval_time(Time, CompareTime) when Time =< CompareTime ->
    true;
eval_time(_, _) ->
    false.
