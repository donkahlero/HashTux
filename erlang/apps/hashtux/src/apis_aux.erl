-module(apis_aux).

-export([generate_twitter_q_param/2]).


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
