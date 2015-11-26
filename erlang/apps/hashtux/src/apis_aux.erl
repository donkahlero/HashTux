-module(apis_aux).

-export([generate_twitter_q_param/2]).
-export([youtube_to_epoch/1, back_one_week/1, datetime_to_rfc_339/1]).


%% =================================
%%	TWITTER API AUX FUNCTIONS
%% =================================

%% @doc Generates a Twitter Search API 'q' parameter with since and until operators
generate_twitter_q_param (HashTag, [])-> HashTag;
generate_twitter_q_param (HashTag, Timestamp)->
	{SinceParam, UntilParam} = generate_twitter_timeframe(Timestamp),
	HashTag ++ " " ++ SinceParam ++ " " ++ UntilParam.

%% @doc Prepend a zero to a single-character string.
%% 		Return the string if it contains more of than one character.
two_digit_string(StringInt) -> 
	case (length(StringInt)) of
		1 -> "0" ++ StringInt;
		_Other -> StringInt
	end.

%% @doc Gets an epoch timestamp and returns a time interval in
%  	the following format {StartDate, EndDate}
%	StartDate and EndDate are formatted as required by the twitter API	
generate_twitter_timeframe(Timestamp) -> 

	%% Get a 2 days interval
	StartTime = Timestamp - 172800,

	CurrentTimestamp = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(os:timestamp()))-719528*24*3600,
	
	% Twitter SEARCH API has a 1-week limit
	% If StartTimestamp is before a week ago, set StartTimestamp equal to one week ago
	StartTimestamp = if
		(StartTime < CurrentTimestamp - 604800) -> 
			io:format("*******Twitter HISTORY request was dated too old. Time Interval adjusted\n"),
			CurrentTimestamp - 604800;
		true -> StartTime
	end,

	%% Set End Timestamp two days later Start Timestamp
	EndTimestamp = StartTimestamp + 172800,

    {{Y1, Month1, Day1}, {_Hour1, _Min1, _Sec1}} = calendar:gregorian_seconds_to_datetime(StartTimestamp),
    Year1 = 1970 + Y1,
    {{Y2, Month2, Day2}, {_Hour2, _Min2, _Sec2}} = calendar:gregorian_seconds_to_datetime(EndTimestamp),
    Year2 = 1970 + Y2,

    SinceParam = "since:" ++ integer_to_list(Year1) ++ "-" ++ two_digit_string(integer_to_list(Month1)) ++ "-" ++ two_digit_string(integer_to_list(Day1)),
    UntilParam = "until:" ++ integer_to_list(Year2) ++ "-" ++ two_digit_string(integer_to_list(Month2)) ++ "-" ++ two_digit_string(integer_to_list(Day2)),

    {SinceParam, UntilParam}.

%% =================================
%%	YOUTUBE DATA API AUX FUNCTIONS
%% =================================

%% @doc Function takes the YouTube representation of time and converts it 
%% to a UNIX epoch timestamp.
youtube_to_epoch(Date) ->

    Year = list_to_integer(string:substr(Date, 1,4)),
    Month = list_to_integer(string:substr(Date, 6,2)),
    Day = list_to_integer(string:substr(Date, 9,2)),
    Hour = list_to_integer(string:substr(Date, 12,2)),
    Min = list_to_integer(string:substr(Date, 15,2)),
    Sec = list_to_integer(string:substr(Date, 18,2)),

    GregDate = {Year, Month, Day},
    Time = {Hour, Min, Sec},
    DateTime = {GregDate, Time},
    calendar:datetime_to_gregorian_seconds(DateTime) - 719528*24*3600.

% @doc returns timestamp 1 week before current time in calendar datetime format
back_one_week({{Year, Month, Day},{Hour, Min, Sec}}) ->
    
    DayDiff = Day - 7,

    if
        % If we got back to previous month
        (DayDiff < 1) -> 
            NewDay = 28 + DayDiff,

            if
                ((Month - 1) < 1) ->
                    NewMonth = 12, 
                    NewYear = Year - 1;
                true ->
                    NewMonth = Month - 1,
                    NewYear = Year
            end,

            {{NewYear, NewMonth, NewDay}, {Hour, Min, Sec}};
        
        % If we DID NOT get back to previous month
        true ->
            NewDay = DayDiff,
            NewMonth = Month,
            NewYear = Year,

            {{NewYear, NewMonth, NewDay}, {Hour, Min, Sec}}
    end.

% @doc Convert a timestamp in calendar datetime format into the RFC 339 format (1970-01-01T00:00:00Z)
datetime_to_rfc_339({{Year, Month, Day},{Hour, Min, Sec}}) ->

    StringYear = integer_to_list(Year),
    StringMonth = two_digit_string(integer_to_list(Month)),
    StringDay = two_digit_string(integer_to_list(Day)),
    StringHour = two_digit_string(integer_to_list(Hour)),
    StringMin = two_digit_string(integer_to_list(Min)),
    StringSec = two_digit_string(integer_to_list(Sec)),

    StringYear ++ "-" ++ StringMonth ++ "-" ++ StringDay ++ "T" ++ StringHour ++ ":" ++ StringMin ++ ":" ++ StringSec ++ "Z".

youtube_get_after_param() -> 
	Timestamp = dateconv:get_timestamp(),
	io:format("TIMESTAMP is ~p~n", [Timestamp]),
	Timestamp.

