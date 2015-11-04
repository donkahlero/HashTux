%% @author jerker
%% @doc This module handles extracting relevant user habit data from
%%		a request to the http_handler and giving it as a json object
%%		to our DB.


-module(user_habits).

%% ====================================================================
%% API functions
%% ====================================================================
-export([store/1]).

store(Req) ->
	% Extract habit data
	HabitData = extract(Req),

	% Send it to the DB server
	Ref = gen_server:call(db_serv, {add_habit_doc, HabitData}),
	receive 
		{Ref, _} ->
			% Regardless of what is returned from this DB worker, return ok
			ok
		after 1000 ->
			% Timeout also means we move on.
			% Is there a risk of messages clogging up? I could do a flush in
			% this method but maybe that would affect usign code.
			% Later it could be better to have this module as a server,
			% right now it's very small though so let's wait with this
			ok
	end.

extract(Req) ->	 
	% Extract the relevant variables (as binaries) from the request
	{TimeStamp, _} = cowboy_req:qs_val(<<"timestamp">>, Req, <<"unknown">>),
	{SessionID, _} = cowboy_req:qs_val(<<"session_id">>, Req, <<"unknown">>),
	{IPAddress, _} = cowboy_req:qs_val(<<"ip_address">>, Req, <<"unknown">>),
	{Language, _} = cowboy_req:qs_val(<<"language">>, Req, <<"unknown">>),
	{UserAgent, _} = cowboy_req:qs_val(<<"user_agent">>, Req, <<"unknown">>),
	{Limit, _} = cowboy_req:qs_val(<<"limit">>, Req, <<"unknown">>),
	{Services, _} = cowboy_req:qs_val(<<"services">>, Req, <<"unknown">>),
	
	% Convert binary representation of TimeStamp to int
	TimeStampString = binary:bin_to_list(TimeStamp),
	TimeStampInt = list_to_integer(TimeStampString),
	
	% Put it together as a list of key-value pairs suitable for DB storage. 
	% Return this
	[{<<"timestamp">>, TimeStampInt},
	 	{<<"session_id">>, SessionID},
		{<<"ip_address">>, IPAddress},
		{<<"language">>, Language},
		{<<"user_agent">>, UserAgent},
		{<<"limit">>, Limit},
		{<<"services">>, Services}].		


%% ====================================================================
%% Internal functions
%% ====================================================================


