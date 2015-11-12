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
					false  -> {content_type, []}
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
	R = ig_search:search(Term),
	{_, L} = ContType,
	filter_insta(R, L);
search_services({twitter, {Term, ContType, Lang}}) ->
	twitter_search:search_hash_tag(Term, [ContType, Lang]);
search_services({youtube, {Term, _ContType, _Lang}}) ->
	youtube_search:search(Term, []).


%%
%% @doc Returns a list of the services to search for. If an empty list is
%% passed as argument, returns all possible services. Otherwise returns 
%% the list passed.
%%
get_services([]) ->
	[instagram, twitter, youtube];
get_services(L)  -> 
	L.


%% 
%% @doc Checks for the options for which to filter Instagram results. The
%% options can be 'image' and 'video'. Calls filter_insta_res/2 if needed.
%% 
filter_insta(Res, []) -> Res;
filter_insta(Res, L)  ->
	case {lists:member(image, L), lists:member(video, L)} of
		{true, true}   -> Res;
 		{true, false}  -> filter_insta_res(Res, image);
		{false, true}  -> filter_insta_res(Res, video)
	end.


%%
%% @doc Filters the results returned from Instagram based on the key 
%% passed. The key is an atom. Returns a list containing the results 
%% for which the key matches the key atom returned from get_key_atom/1. 
%% 
filter_insta_res([], _Key)	 -> [];
filter_insta_res(List, Key) ->
	[N || N <- List, get_key_atom(N) == Key]. 


%%
%% @doc Gets the value from the key-value pair with key content_type
%% in the results from Instagram. Returns this value name as atom or 
%% the atom 'no_atom' if not found.
%%
get_key_atom(List) ->
	X = case lists:keyfind(<<"content_type">>, 1, List) of
			{_K, V} -> list_to_atom(binary_to_list(V));
			false	-> no_atom
		end,
	X.




