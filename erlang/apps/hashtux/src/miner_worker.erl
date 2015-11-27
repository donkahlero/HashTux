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
	send_results(Pid, Results, Term, Options),
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
send_results(Pid, [], Term, Options) ->
	io:format("Results returned from search: ~p~n", [[]]),	
	case get_value(request_type, Options) of
		<<"search">> -> 
			gen_server:call(db_serv, {add_doc, [get_no_results(Term, Options)]}),
			Pid ! {self(), []};
		<<"update">> ->
			gen_server:call(db_serv, {add_doc, [get_no_results(Term, Options)]}),
			Pid ! {self(), []};
		<<"heartbeat">> ->
			gen_server:call(db_serv, {add_doc, [get_no_results(Term, Options)]})
	end;
send_results(Pid, Results, _Term, Options) ->

	FilteredResults = get_aggregated_results(Results, filtered),
	UnfilteredResults = get_aggregated_results(Results, unfiltered),

	case get_value(request_type, Options) of
		<<"search">> -> 
			io:format("WORKER: Writing to db...~n"),
			gen_server:call(db_serv, {add_doc, [UnfilteredResults]}),
			Pid ! {self(), FilteredResults};
		<<"update">> ->
			gen_server:call(db_serv, {add_doc, [UnfilteredResults]}),
			Pid ! {self(), FilteredResults};
		<<"heartbeat">> ->
			ok
	end.
			

%% 
% no options
run_search(Term, []) -> 
	io:format("WORKER: Running search...~n"),
	ContType = {content_type, get_cont_type()},
	Lang = {language, []},
	HistoryTimestamp = {history_timestamp, []},
	L = get_results(Term, get_services([]), ContType, Lang, HistoryTimestamp),
	lists:append(L);
% with options
run_search(Term, Options) ->
	io:format("WORKER: Running search...~n"),
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
	L = get_results(Term, Services, ContType, Lang, HistoryTimestamp),
	lists:append(L).


%%
%% @doc Returns a list with the results from searching the different services 
%% available. The search is performed in parallel for each service.
%%
get_results(Term, Services, ContType, Lang, HistoryTimestamp) ->
	io:format("WORKER: Getting results...~n"),
	F = fun(Pid, X) -> spawn(fun() -> 
									Pid ! {self(), 
									search_services({X, {Term, ContType, Lang, HistoryTimestamp}})} 
							  end) 
		end,
	[receive {R, X} -> X end || R <- [F(self(), N) || N <- Services]].


%%
%% @doc Calls the appropriate search services to perform a search.
%%
search_services({instagram, {Term, ContType, _Lang, _HistoryTimestamp}}) ->
	io:format("WORKER: Calling ig_search...~n"),
	ig_search:search(Term, [ContType]);
search_services({twitter, {Term, ContType, Lang, HistoryTimestamp}}) ->
	io:format("WORKER: Calling twitter_search...~n"),
	twitter_search:search_hash_tag(Term, [ContType, Lang, HistoryTimestamp]);
search_services({youtube, {Term, ContType, Lang, HistoryTimestamp}}) ->
	io:format("WORKER: Calling youtube_search...~n"),
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
get_no_results(Term, Options) ->
	[ {<<"results">>, <<"no">>},
	  {<<"search_term">>, list_to_binary(Term)},
	  {<<"timestamp">>, dateconv:get_timestamp()},
	  {<<"options">>, Options} ].


%% 
get_cont_type() ->
	[<<"image">>, <<"video">>, <<"text">>].

%% @author Marco Trifance
%% @doc Gets a list of raw results (filtered and unfiltered for all social medias) and return a list
%%		for the specified FilterType (filtered/unfiltered)
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

%%
get_value(_Key, [])   -> [];
get_value(_Key, null) -> [];
get_value(Key, List)  ->
	case lists:keyfind(Key, 1, List) of
		{_K, V}	-> V;
		false 	-> []
	end.