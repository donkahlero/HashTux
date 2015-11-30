%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @doc Module for converting dates and times to other formats.
%% @version 0.2
%% -----------------------------------------------------------------------------
%% | Sprint 2 // v0.1:                                                         |
%% | Initially added this module to convert the twitter date.                  |
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v.02:                                                         |
%% | Added a function for getting UNIX epoch in seconds.                       |
%% -----------------------------------------------------------------------------
-module(dateconv).

-export([twitter_to_epoch/1, get_timestamp/0]).

%% @doc Function takes the twitter representation of time and converts it 
%% to a UNIX epoch timestamp.
twitter_to_epoch(Date) ->
    Year = list_to_integer(string:substr(Date, 27)),
    Month = month_to_num(string:substr(Date, 5, 3)),
    Day = list_to_integer(string:substr(Date, 9, 2)),
    Hour = list_to_integer(string:substr(Date, 12, 2)),
    Min = list_to_integer(string:substr(Date, 15, 2)),
    Sec = list_to_integer(string:substr(Date, 18, 2)),
    GregDate = {Year, Month, Day},
    Time = {Hour, Min, Sec},
    DateTime = {GregDate, Time},
    calendar:datetime_to_gregorian_seconds(DateTime) - 719528*24*3600.

%% @doc Helperfunction converting the month to an int.
month_to_num ("Jan") ->
    1;
month_to_num ("Feb") ->
    2;
month_to_num ("Mar") ->
    3;
month_to_num ("Apr") ->
    4;
month_to_num ("May") ->
    5;
month_to_num ("Jun") ->
    6;
month_to_num ("Jul") ->
    7;
month_to_num ("Aug") ->
    8;
month_to_num ("Sep") ->
    9;
month_to_num ("Oct") ->
    10;
month_to_num ("Nov") ->
    11;
month_to_num ("Dec") ->
    12.

get_timestamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(erlang:timestamp()))-719528*24*3600.
