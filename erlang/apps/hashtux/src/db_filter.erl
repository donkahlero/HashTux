%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Module repsonsible for filtering database results. The functions always
%% take lists and necessary information as arguments and filter the list
%% corresponding to that.
%% @version 0.1
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v0.1:                                                         |
%% | Added initial version of this module.                                     |
%% | Module can handle the following filters:                                  |
%% | - content_type, language and service                                      |
%% -----------------------------------------------------------------------------
-module(db_filter).

-export([content_type/2, language/2, service/2, order_by_value/1, limit_result/2]).
-export([group_by_subkey/1, check_results/2, in_timestamp/3]).

%% @doc Function checking if the miners cannot find something or there is just 
%% nothing cached yet.
check_results([[{<<"results">>, <<"no">>}, {<<"search_term">>, _}, {<<"timestamp">>, _}, {<<"options">>, Opt}] | _], Opts) ->
    case(lists:usort(foreach_opt(Opts, Opt, []))) of
	[true] ->
	    no_miner_res;
	_ -> []
    end;
check_results(_, _) ->
    [].

%% @doc Function going through all options.
foreach_opt([], _, Res) ->
    Res;
foreach_opt([X|Xs], Opt, Res) ->
    foreach_opt(Xs, Opt, [in_options(Opt, X) | Res]).

%% @doc Function checking if current option is part of general options.
in_options([], _) ->
    false;
in_options([{_, Val}|_], {_, Val}) ->
    true;
in_options([_|Xs], Opt) ->
    in_options(Xs, Opt).

%% @doc Function filtering for the type of content.
%% This can be image, video or text.
%% Several types within one search are possible.
content_type(L, CTypes) ->
    content_type(L, CTypes, []).

content_type([], _CTypes, Res) ->
    Res;
content_type([X|Xs], CTypes, Res) ->
    case (lists:keyfind(<<"content_type">>, 1, X)) of
	{<<"content_type">>, CType} ->
	    case (is_ctype(CTypes, CType)) of
		false ->
		    content_type(Xs, CTypes, Res);
		true ->
		    content_type(Xs, CTypes, [X|Res])
	    end;
	false ->
	    content_type(Xs, CTypes, Res)
    end.

%% @doc Helperfunction checking for the list of types.
is_ctype([], _CurrentType) ->
    false;
is_ctype([CurrentType|_Xs], CurrentType) ->
    true;
is_ctype([_X|Xs], CurrentType) ->
    is_ctype(Xs, CurrentType).

%% @doc Function filtering for the language of the content.
%% Just one language is possible here.
language(L, Lang) ->
    language(L, Lang, []).

language([], _Lang, Res) ->
    Res;
language([X|Xs], Lang, Res) ->
    CurrentLang = lists:keyfind(<<"language">>, 1, X),
    %%BinLang = binary:list_to_bin(Lang),
    case (CurrentLang) of
	{_, Lang} ->
	    language(Xs, Lang, [X|Res]);
	_ ->
	    language(Xs, Lang, Res)
    end.

%% @doc Function for filtering the content for the service.
%% One or more services are possible: twitter, instagram and
%% youtube.
service(L, Services) ->
    service(L, Services, []).

service([], _Services, Res) ->
    Res;
service([X|Xs], Services, Res) ->
    case (lists:keyfind(<<"service">>, 1, X)) of
	{<<"service">>, Service} ->
	    case(is_service(Services, Service)) of
		true ->
		    service(Xs, Services, [X|Res]);
		false ->
		    service(Xs, Services, X)
	    end;
	false ->
	    service(Xs, Services, Res)
    end.

%% @doc Helperfunction for the service filter.
is_service([], _CurrentService) ->
    false;
is_service([CurrentService|_Xs], CurrentService) ->
    true;
is_service([_X|Xs], CurrentService) ->
    is_service(Xs, CurrentService).

%% @doc
in_timestamp(L, StartTime, EndTime) ->
    in_timestamp(L, StartTime, EndTime, []).

in_timestamp([], _, _, Res) ->
    Res;
in_timestamp([X|Xs], StartTime, EndTime, Res) ->
    {<<"timestamp">>, TimeStamp} = lists:keyfind(<<"timestamp">>, 1, X),
    case(timeeval(TimeStamp, StartTime, EndTime)) of
	true ->
	    in_timestamp(Xs, StartTime, EndTime, [X | Res]);
	false ->
	    in_timestamp(Xs, StartTime, EndTime, Res)
    end.

timeeval(TimeStamp, StartTime, _) when TimeStamp >= StartTime ->
    true;
timeeval(TimeStamp, _, EndTime) when TimeStamp =< EndTime ->
    true;
timeeval(_, _, _) ->
    false.

%% @doc Function ordering mapreduce results by their value
order_by_value(L) ->
    lists:reverse(lists:keysort(2,
      [{Key, Value} || [{<<"key">>, Key}, {<<"value">>, Value}] <- L])).

%% @doc Limits a list to be as long as the Num says
limit_result(Num, L) ->
	{R, _} = lists:split(Num, L),
	R.

%%
group_by_subkey(L) ->
    KVs = [{Key, Value} ||
	  [{<<"key">>, [_Timestamp, Key]}, {<<"value">>, Value}] <- L],
    [[{binary:list_to_bin("key"), Key}, {binary:list_to_bin("value"), Value}] ||
    {Key, Value} <- sum_values(lists:keysort(1, KVs), [])].

sum_values([], Res) ->
    Res;
sum_values([{X, N} | Xs], [{X, M} | Ys]) ->
    sum_values(Xs, [{X, N+M} | Ys]);
sum_values([{X, N} | Xs], Y) ->
    sum_values(Xs, [{X, N} | Y]).
