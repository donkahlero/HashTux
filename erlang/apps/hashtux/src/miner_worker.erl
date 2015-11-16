-module(miner_worker).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0]).


%%% ============================================================================
%%% PUBLIC API
%%% ============================================================================

start_link() ->
	io:format("STARTING:miner_worker~n"),
	gen_server:start_link(?MODULE, [], []).


%%% ============================================================================
%%% CALLBACK FUNCTIONS
%%% ============================================================================


%%
init([]) -> 
	{ok, []}.


%%
terminate(_Reason, _State) -> ok.


%% 
code_change(_PrevVersion, State, _Extra) -> 
	{ok, State}.


%%
handle_info(_Msg, S) -> 
	{noreply, S}.


%%
%% no options
handle_cast({{Pid, _Ref}, Term, Options}, State) ->
	% get results
	Results = run_search(Term, Options),
	% send to original caller								
	Pid ! {self(), Results},	
	io:format("FINISHED:worker [~p]~n", [self()]),
	% stop this worker
	{stop, normal, State};
handle_cast(_Request, State) ->
	{stop, normal, State}.


%%
handle_call(_Request, _From, S) -> 
	{noreply, S}.


%%% ============================================================================
%%% PRIVATE FUNCTIONS
%%% ============================================================================


%%
%send_results(Pid, Results, Term, Options) ->
%	case get_value(request_type, Options) of
%		<<"search">> ->
			


%% 
% no options
run_search(Term, []) -> 
	ContType = {content_type, []},
	Lang = {language, []},
	L = get_results(Term, get_services([]), ContType, Lang),
	lists:append(L);
% with options
run_search(Term, Options) ->
	% get the options
	Services = case lists:keyfind(service, 1, Options) of
					{_K1, V1} -> get_services(V1);
					false  -> get_services([])
			   end,
	ContType = case lists:keyfind(content_type, 1, Options) of
					{K2, V2} -> {K2, V2};
					false  -> {content_type, 
								[<<"image">>, <<"video">>, <<"text">>]}
			   end,
	Lang = case lists:keyfind(language, 1, Options) of
				{K3, V3} -> {K3, V3};
				false  -> {language, []}
		   end,
	L = get_results(Term, Services, ContType, Lang),
	lists:append(L).


%%
%% @doc Returns a list with the results from searching the different services 
%% available. The search is performed in parallel for each service.
%%
get_results(Term, Services, ContType, Lang) ->
	F = fun(Pid, X) -> spawn(fun() -> 
									Pid ! {self(), 
									search_services({X, {Term, ContType, Lang}})} 
							  end) 
		end,
	[receive {R, X} -> X end || R <- [F(self(), N) || N <- Services]].


%%
%% @doc Calls the appropriate search services to perform a search.
%%
search_services({instagram, {Term, ContType, _Lang}}) ->
	ig_search:search(Term, [ContType]);
search_services({twitter, {Term, ContType, Lang}}) ->
	twitter_search:search_hash_tag(Term, [ContType, Lang]);
search_services({youtube, {Term, ContType, Lang}}) ->
	youtube_search:search(Term, [ContType, Lang]).


%%
%% @doc Returns a list of the services to search for. If an empty list is
%% passed as argument, returns all possible services. Otherwise returns 
%% the list passed.
%%
get_services([]) ->
	[instagram, twitter, youtube];
get_services(L)  -> 
	[list_to_atom(binary_to_list(X)) || X <- L].


%%
get_value(_Key, [])  -> [];
get_value(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		{_K, V}	-> V;
		false 	-> []
	end.



