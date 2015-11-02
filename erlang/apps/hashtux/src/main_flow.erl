-module(main_flow).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3]).

start_link() ->
    gen_server:start_link({local, main_flow}, main_flow, [], []).

init(_Args) ->
	io:format("Started the main flow controller.~n~n"),
    {ok, []}.


handle_info(_Msg, State) -> {noreply, State}.


handle_call({search, Term}, From, State) -> 
	io:format("Term: ~p~n", [Term]),
	
	% Update the database with search term / session data for this request
	% ...
	% ...
	% ...
	
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
	
	%
	% Decide on some way to forward the cache to the request / miner code
	%
	
	% Make a miner call for the term
	{ok, MinerPid} = miner_server:search(Term, none),
	receive 
		{MinerPid, Y, Z} ->
			{reply, Y, State}
		after 1000 ->
			{reply, "Miner timeout!", State}
	end.
	
	%io:format("MSG: ~p~n", [X]),
	%{reply, ok, State}.

				