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
init([]) -> {ok, []}.


%%
terminate(_Reason, _State) -> ok.


%% 
code_change(_PrevVersion, _State, _Extra) -> ok.


%%
handle_info(_Msg, S) -> {noreply, S}.


%%
%% no options
handle_cast({{Pid, _Ref}, Term, []}, State) ->
	% get instagram results
	IGRes = ig_search:search(Term, []),
	%get twitter results
	TWRes = twitter_search:search_hash_tag(Term, []),
	% concatenate final results
	Results = IGRes ++ TWRes,
	% send to original caller								
	Pid ! {self(), Results},	
	io:format("FINISHED:worker [~p]~n", [self()]),
	% stop this worker
	{stop, normal, State};

%% with options
handle_cast({{Pid, _Ref}, Term, Options}, State) ->
	% get the options
	Services = case lists:keyfind(service, 1, Options) of
					{K1, V1} -> {K1, V1};
					false  -> {service, []}
			   end,
	ContType = case lists:keyfind(content_type, 1, Options) of
					{K2, V2} -> {K2, V2};
					false  -> {content_type, []}
			   end,
	Lang = case lists:keyfind(language, 1, Options) of
				{K3, V3} -> {K3, V3};
				false  -> {language, []}
		   end,
	% get the search results
	Results = search_services(Term, Services, ContType, Lang), 
	% send to original caller								
	Pid ! {self(), Results},
	io:format("FINISHED:worker [~p]~n", [self()]),
	% stop this worker
	{stop, normal, State}.


%%
handle_call(_Request, _From, S) -> {noreply, S}.


%%
% search all services (twitter, instagram, etc.)
search_services(Term, {service, []}, ContType, Lang) ->
	R = ig_search:search(Term, []),
	{_, L} = ContType, 
	IGRes = filter_insta(R, L),
	TWRes = twitter_search:search_hash_tag(Term, [ContType, Lang]),
	IGRes ++ TWRes;
% search based on options
search_services(Term, Services, ContType, Lang) ->
	% get the list of services
	{service, S} = Services,
	% get instagram results
	IGRes = case lists:member(instagram, S) of
				true  -> 
					R = ig_search:search(Term, []),
					{_, L} = ContType,
					filter_insta(R, L);
				false -> []
			end,
	% get twitter results
	TWRes = case lists:member(twitter, S) of
				true  -> twitter_search:search_hash_tag(Term, [ContType, Lang]);
				false -> []
			end,
	IGRes ++ TWRes.


%% 
filter_insta(Res, []) -> Res;
filter_insta(Res, L)  ->
	case {lists:member(image, L), lists:member(video, L)} of
		{true, true}  -> Res;
		{true, false} -> filter_insta_res(Res, image);
		{false, true} -> filter_insta_res(Res, video)
	end.


%%
filter_insta_res([], _Key)	 -> [];
filter_insta_res([H|T], Key) -> 
	X = case lists:keyfind(<<"content_type">>, 1, H) of
			{_K, V} -> list_to_atom(binary_to_list(V));
			false	-> false
		end,
	if 
		X == Key -> [H | filter_insta_res(T, Key)];
		X /= Key -> filter_insta_res(T, Key)
	end. 











			
	
	











