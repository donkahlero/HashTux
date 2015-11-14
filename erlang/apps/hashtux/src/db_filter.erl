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
-export([group_by_subkey/1]).

%% @doc Function filtering for the type of content.
%% This can be image, video or text.
%% Several types within one search are possible.
content_type(L, CTypes) ->
    content_type(L, CTypes, []).

content_type([], _CTypes, Res) ->
    Res;
content_type([X|Xs], CTypes, Res) ->
    {_, CurrentType} = lists:keyfind(<<"content_type">>, 1, X),
    case (is_ctype(CTypes, CurrentType)) of
	false ->
	    content_type(Xs, CTypes, Res);
	true ->
	    content_type(Xs, CTypes, [X|Res])
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
    BinLang = binary:list_to_bin(Lang),
    case (CurrentLang) of
	{_, BinLang} ->
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
    {_, CurrentService} = lists:keyfind(<<"service">>, 1, X),
    case (is_service(Services, CurrentService)) of
        false ->
	    service(Xs, Services, Res);
	true ->
	    service(Xs, Services, [X|Res])
    end.

%% @doc Helperfunction for the service filter.
is_service([], _CurrentService) ->
    false;
is_service([CurrentService|_Xs], CurrentService) ->
    true;
is_service([_X|Xs], CurrentService) ->
    is_service(Xs, CurrentService).

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
