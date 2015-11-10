%% @author jerker
%% @doc @todo Add description to http_option_parser.


-module(http_option_parser).

%% ====================================================================
%% API functions
%% ====================================================================
-export([parse_options/1]).

parse_options(Req) -> 
	{ServicesJSON, _} = cowboy_req:qs_val(<<"services">>, Req, []),
	io:format("ServicesJSON: ~p~n", [ServicesJSON]),
	[].

	

%% ====================================================================
%% Internal functions
%% ====================================================================


