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



%% ====================================================================
%% Internal functions
%% ====================================================================