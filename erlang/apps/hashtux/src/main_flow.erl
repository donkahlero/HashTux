-module(main_flow).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3]).

start_link() ->
    gen_server:start_link({local, main_flow}, main_flow, [], []).

init(_Args) ->
	io:format("Started the main flow controller.~n~n"),
    {ok, []}.

handle_call({search, Term}, From, State) -> 
	io:format("Term: ~p~n", [Term]),
	
	% Make a database call for the term
	%Ref = gen_server:call(db_serv, {get_cont, Term}),
	%receive 
	%	{Ref, [{<<"error">>,<<"not_found">>},{<<"reason">>,<<"missing">>}]} ->
	%		{reply, "Nothing found!", State};
	%	{Ref, Res} ->
	%		{reply, Res, State}
	%	after 1000 ->
	%		{reply, "DB timeout!", State}
	%end.
	
	% Make a miner call for the term
	{ok, Pid2, Ref2, News2} = miner_server:search(Term, none),
	receive 
		{Pid2, Y, Z} ->
			{reply, Y, State}
		after 1000 ->
			{reply, "Miner timeout!", State}
	end.
	
	%io:format("MSG: ~p~n", [X]),
	%{reply, ok, State}.

				