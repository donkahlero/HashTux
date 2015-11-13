%% @author jerker
%% @doc This module handles extracting relevant user habit data from
%%		a request to the http_handler and giving it as a json object
%%		to our DB.


-module(user_habits).

%% ====================================================================
%% API functions
%% ====================================================================
-export([store/3]).

store(Term, Options, UserHabitData) ->
	% Extract habit data and concatenate options at the end
	%FullHabitData = extract(Term, Req, Options),

	% Concatenate the two lists UserHabitData and Options
	FullHabitData = lists:append(UserHabitData, Options),

	% Send it to the DB server
	Ref = gen_server:call(db_serv, {add_habit_doc, FullHabitData}),
	receive 
		{Ref, _} ->
			% Regardless of what is returned from this DB worker, return ok
			ok
		after 1000 ->
			% Timeout also means we move on.
			% Is there a risk of messages clogging up? I could do a flush in
			% this method but maybe that would affect usign code.
			% Maybe later it could be better to have this module as a server,
			% right now it's very small though so let's wait with this
			ok
	end.

extract(Term, Req, Options) ->	 
	% Extract the relevant variables (as binaries) from the request
	{TimeStamp, _} = cowboy_req:qs_val(<<"timestamp">>, Req, <<"unknown">>),
	{SessionID, _} = cowboy_req:qs_val(<<"session_id">>, Req, <<"unknown">>),
	{IPAddress, _} = cowboy_req:qs_val(<<"ip_address">>, Req, <<"unknown">>),
	{Browser, _} = cowboy_req:qs_val(<<"browser">>, Req, <<"unknown">>),
	{BrowserVersion, _} = cowboy_req:qs_val(<<"browser_version">>, Req, <<"unknown">>),
	{Platform, _} = cowboy_req:qs_val(<<"platform">>, Req, <<"unknown">>),
	
	% Convert binary representation of TimeStamp to int
	TimeStampString = binary:bin_to_list(TimeStamp),
	TimeStampInt = list_to_integer(TimeStampString),
	
	% Put it together as a list of key-value pairs suitable for DB storage. 
	HabitData = [{<<"search_term">>, list_to_binary(Term)},
	 	{<<"timestamp">>, TimeStampInt},
	 	{<<"session_id">>, SessionID},
		{<<"ip_address">>, IPAddress},
		{<<"browser">>, Browser},
		{<<"browser_version">>, BrowserVersion},
		{<<"platform">>, Platform}],
	
	% Add the options at the end of the above list
	HabitData ++ Options.


%% ====================================================================
%% Internal functions
%% ====================================================================


