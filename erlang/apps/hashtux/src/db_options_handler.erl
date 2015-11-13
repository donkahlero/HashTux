%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% %% @doc Handles operations on options for the db_readers
%% %% %% %% @version 0.1
-module(db_options_handler).

-export([order_options/1, search_opt/2, pre_search_opt/1, pre_search_opt/2]).

%% -----------------------------------------------------------------------------
%% | Sprint 4                                                                  |
%% | Version 0.1                                                               |
%% | This module is created for the purpose of handling options for the        |
%% | advanced search and as well for the user statistics. It orders_options,   |
%% | search_opt, pre_search_opt.                                               |
%% -----------------------------------------------------------------------------

%% @doc This orders the list of options so that the limit is last.
order_options(Opt) ->
	order_options(Opt, []).
order_options([], L) ->
	L;
order_options([{limit, Lim}| T], L) ->
	R = L ++ [{limit, Lim}],
	order_options(T, R);
order_options([H| T], L) ->
	R = [H] ++ L,
order_options(T, R).

%% @doc This returns a new list of json objects from the differnt options.
search_opt([], L) ->
	L;
search_opt([{content_type, CTs}| T], L) ->
	R = db_filter:content_type(L, CTs),
	search_opt(T, R);
search_opt([{service, Services}| T], L) ->
	R = db_filter:service(L, Services),
	search_opt(T, R);
search_opt([{language, Langs}| T], L) ->
	R = db_filter:language(L, Langs),
	search_opt(T, R);
search_opt([{limit, Num}| _], L) ->
	R = db_filter:limit_result(Num, L),
	R.

%% @doc This returns an url string for the quering of the db.
pre_search_opt(L) -> 
       pre_search_opt(L, []).	
pre_search_opt([], L) ->
	L;
pre_search_opt([{time}| T], L) ->
	R = L ++ "startkey=[0" ++ ",\"a\"]&endkey=[99999999999999999999999999" ++ ",\"z\"]",
	pre_search_opt(T, R);
pre_search_opt([{time, Start, End}| T], L) ->
	R = L ++ "startkey=[" ++ integer_to_list(Start) ++ ",\"a\"]&endkey=[" ++ integer_to_list(End) ++ ",\"z\"]",
	pre_search_opt(T, R);
pre_search_opt([_| T], L) ->
	pre_search_opt(T, L).

