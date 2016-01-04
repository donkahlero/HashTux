%% @author Jerker Ersare <jerker@soundandvision.se>
%% @doc This module contains a helper function to forward user habit data
%% (concatenated with request options) to the user habit database.
%% NOTE this file has shrunk lately, could be refecatored away probably :)


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
		after 1500 ->
			% Timeout also means we move on.
			% (TODO: Consider the risk of messages clogging up on timeout!)
			ok
	end.