-module(miner_worker).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0, stop/0]).




%%% ============================================================================
%%% PUBLIC API
%%% ============================================================================


%% 
%% @doc Starts the worker.
%%
start_link() ->
	io:format("MINER_WORKER: Starting...~n"),
	gen_server:start_link(?MODULE, [], []).


%%
%% @doc Stops the worker.
%%
stop() ->
	gen_server:cast(?MODULE, stop).




%%% ============================================================================
%%% CALLBACK FUNCTIONS
%%% ============================================================================


%%
%% @doc Handles the initialisation the worker.
%%
init([]) -> 
	{ok, []}.


%%
%% @doc Handles the termination of the worker.
%%
terminate(Reason, _State) -> 
	io:format("MINER_WORKER [~p]: Stopping for reason: ~p~n", [self(), Reason]),	
	ok.


%%
%% @doc Handles code change.
%% 
code_change(_PrevVersion, State, _Extra) -> 
	{ok, State}.


%%
%% @doc Handles messages sent to the worker.
%%
handle_info(Msg, State) -> 
	io:format("MINER_WORKER [~p]: Unknown message: ~p~n", [self(), Msg]),
	{stop, normal, State}.


%%
%% @doc Handles casts to the worker. Only casts to stop the worker are 
%% supported.
%%
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(Msg, State) ->
	io:format("MINER_WORKER [~p]: Unknown cast: ~p~n", [self(), Msg]),
	{stop, normal, State}.


%%
%% @doc Handles calls to the worker.
%%
handle_call({{Pid, _Ref}, Term, Options}, _From, State) -> 
	% get results
	Results = run_search(Term, Options),
	% parse_results into filtered and unfiltered
	{FilteredRes, UnfiltereRes} = parse_results(Results),
	% send to original caller	
	send_results(Pid, FilteredRes, UnfiltereRes, Term, Options),
	io:format("MINER_WORKER [~p]: Finished~n", [self()]),
	% stop this worker
	{stop, normal, ok, State}.




%%% ============================================================================
%%% PRIVATE FUNCTIONS
%%% ============================================================================


%%
send_results(Pid, [], _UnfilteredResults, Term, Options) ->
	io:format("MINER_WORKER [~p]: Sending results...~n", [self()]),	
	case aux:get_value(request_type, Options) of
		<<"search">> -> 
			miner_dbwriter:write(get_no_results(Term, Options)),
			Pid ! {self(), []};
		<<"update">> ->
			miner_dbwriter:write(get_no_results(Term, Options)),
			Pid ! {self(), []};
		<<"heartbeat">> ->
			miner_dbwriter:write(get_no_results(Term, Options))
	end;
send_results(Pid, FilteredResults, UnfilteredResults, _Term, Options) ->
	io:format("MINER_WORKER [~p]: Sending results...~n", [self()]),
	case aux:get_value(request_type, Options) of
		<<"search">> -> 
			miner_dbwriter:write(UnfilteredResults),
			Pid ! {self(), FilteredResults};
		<<"update">> ->
			miner_dbwriter:write(UnfilteredResults),
			Pid ! {self(), FilteredResults};
		<<"heartbeat">> ->
			ok
	end.
			

%% 
%% @doc Checks the options passed and runs a search accordingly.
%%
run_search(Term, Options) ->
	io:format("MINER_WORKER [~p]: Running search...~n", [self()]),
	% get the options
	Services = case lists:keyfind(service, 1, Options) of
					{_K1, V1} -> get_services(V1);
					false  -> get_services([])
			   end,
	ContType = case lists:keyfind(content_type, 1, Options) of
					{K2, V2} -> {K2, V2};
					false  -> {content_type, get_cont_type()}
			   end,
	Lang = case lists:keyfind(language, 1, Options) of
				{K3, V3} -> {K3, V3};
				false  -> {language, []}
		   end,
	HistoryTimestamp = case lists:keyfind(history_timestamp, 1, Options) of
				{K4, V4} -> {K4, V4};
				false  -> {history_timestamp, []}
		   end,
	% get the results
	L = get_results(Term, Services, ContType, Lang, HistoryTimestamp),
	% return results properly formatted
	lists:append(L).


%%
%% @doc Returns a list with the results from searching the different services 
%% available. The search is performed in parallel for each service.
%%
get_results(Term, Services, ContType, Lang, HistoryTimestamp) ->
	io:format("MINER_WORKER [~p]: Getting results...~n", [self()]),
	F = fun(Pid, X) -> 
			spawn(fun() -> 
						Pid ! {self(), 
						search_services({X, {Term, ContType, Lang, HistoryTimestamp}})} 
			end) 
		end,
	[ receive {R, X} -> X end || R <- [F(self(), N) || N <- Services] ].


%%
%% @doc Calls the appropriate search services to perform a search.
%%
%%% Instagram search.
search_services({instagram, {Term, ContType, _Lang, _HistoryTimestamp}}) ->
	ig_search:search(Term, [ContType]);
%%% Twitter search.
search_services({twitter, {Term, ContType, Lang, HistoryTimestamp}}) ->
	twitter_search:search_hash_tag(Term, [ContType, Lang, HistoryTimestamp]);
%%% YouTube search.
search_services({youtube, {Term, ContType, Lang, HistoryTimestamp}}) ->
	youtube_search:search(Term, [ContType, Lang, HistoryTimestamp]).


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
%% @doc Returns the no results options to be written to database.
%%
get_no_results(Term, Options) ->
	io:format("MINER_WORKER [~p]: No results options: ~p~n", [self(), Options]),
	[ [{<<"results">>, <<"no">>},
	   {<<"search_term">>, list_to_binary(Term)},
	   {<<"timestamp">>, dateconv:get_timestamp()},
	   {<<"options">>, Options}] ].


%% 
%% @doc Returns all the content types to search for.
%%
get_cont_type() ->
	[<<"image">>, <<"video">>, <<"text">>].


%% @author Marco Trifance
%% @doc Helper function for parse_results/1
%%		Gets a list of raw results (filtered and unfiltered for all social medias) and return 
%% 		an aggregated list from all three social medias for the specified FilterType (filtered/unfiltered)
get_aggregated_results(RawList, FilterType) ->
	get_aggregated_results(RawList, FilterType, []).

get_aggregated_results([], _FilterType, AggregatedResult) -> AggregatedResult;
get_aggregated_results([H|T], FilterType, AggregatedResult) ->
	case (is_type(H, FilterType)) of
		true -> 
			{_Key, Value} = H,
			NewAggregatedResult = AggregatedResult ++ Value,
			get_aggregated_results(T, FilterType, NewAggregatedResult);
		false ->
			get_aggregated_results(T, FilterType, AggregatedResult)
	end.

%% @author Marco Trifance
%% @doc Helper function for get_aggregated_results/3
is_type({FilterType, _Any}, FilterType) -> true;
is_type({_OtherType, _Any}, _FilterType) -> false.

%% @author Marco Trifance
%% @doc Gets a list of raw results (filtered and unfiltered for all social medias) and returns 
%% 		a tuple of two lists containing filtered and unfiltered results for all social medias
parse_results(Results) -> 

	FilteredResults = get_aggregated_results(Results, filtered),
	UnfilteredResults = get_aggregated_results(Results, unfiltered),

	{FilteredResults, UnfilteredResults}.

