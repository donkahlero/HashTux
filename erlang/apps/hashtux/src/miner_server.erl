-module(miner_server).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0, stop/0]).
-export([search/2, heartbeat/2]).

%% Record for keeping track of the state, namely, for how many workers
%% are currently operating so we know when to start distributing the 
%% work load.
-record(state, {limit=1000,
				 refs,
				 queue=queue:new()}).




%%% ============================================================================
%%% PUBLIC API
%%% ============================================================================


%% 
%% @doc Starts the server.
%%
start_link() ->	
	io:format("MINER_SERVER: Starting...~n"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%
%% @doc Stops the server.
%%
stop() ->
	gen_server:cast(?MODULE, stop).


%%
%% @doc Searches for the specified term according to the options provided.
%%
search(Term, Options) ->
	gen_server:call(?MODULE, {search, Term, Options}).


%%
%% @doc Handles heartbeat requests from client.
%%
heartbeat(Term, Options) ->
	gen_server:cast(?MODULE, {heartbeat, Term, Options}).




%%% ============================================================================
%%% CALLBACK FUNCTIONS
%%% ============================================================================

%% 
%% @doc Handles the initialisation of the server.
%%
init([]) -> 
	{ok, #state{refs=gb_sets:empty()}}.


%%
%% @doc Handles the termination of the server.
%%
terminate(Reason, _State) ->
	io:format("MINER_SERVER: Stopping for reason: ~p~n", [Reason]),
	ok.


%% 
%% @doc Handles code change.
%%
code_change(_PrevVersion, State, _Extra) ->
	{ok, State}.


%%
%% @doc Handles the down message received from the miner worker when it has 
%% finished with the task (or not).
%%
%%% When received a down message from the worker process. 
handle_info({'DOWN', Ref, process, Pid, _}, 
						S=#state{limit=N, refs=Refs}) ->
	io:format("MINER_SERVER: Removing worker [~p] from refs...~n", [Pid]),
	case gb_sets:is_element(Ref, Refs) of
		true ->
			NewRefs = gb_sets:delete(Ref, Refs),
			NewS = S#state{limit=N+1, refs=NewRefs},
			{noreply, NewS};
		false ->
			{noreply, S}
	end;
%%% All other messages -> ignored
handle_info(Msg, State) ->
	io:format("MINER_SERVER: Unknown message: ~p~n", [Msg]),
	{noreply, State}.


%%
%% @doc Handles casts made to the server. Only a cast to stop the server is
%% handled. All other casts are ignored.
%%
%%% Cast to stop the server.
handle_cast(stop, State) ->
	{stop, normal, State};
%%% When heartbeat requested and limit for workers not reached.
handle_cast({heartbeat, Term, Options},
						S=#state{limit=N, refs=R}) when N > 0 ->
	{ok, Pid} = start_worker(),
	Ref = erlang:monitor(process, Pid),
	% cast to worker with {0, 0} instead of the {Pid, Ref}
	% we never send back results to original caller when heartbeat
	% so in this case it doesn't matter
	gen_server:cast(Pid, {{0, 0}, Term, Options}),
	NewS = S#state{limit=N-1, refs=gb_sets:add(Ref, R)},
	{noreply, NewS};
%%% When heartbeat requested and limit for workers reached.
handle_cast({heartbeat, Term, Options}, 
						S=#state{limit=N}) when N =< 0 ->
	{noreply, S};
%%% Other casts ignored.
handle_cast(Msg, State) ->
	io:format("MINER_SERVER: Unknown cast: ~p~n", [Msg]),
	{noreply, State}.


%%
%% @doc Handles calls to the server.
%%
%%% When a search requested and limit for workers running not reached.
handle_call({search, Term, Options}, From, 
						S=#state{limit=N, refs=R}) when N > 0 ->
	{ok, Pid} = start_worker(),
	Ref = erlang:monitor(process, Pid),
	gen_server:cast(Pid, {From, Term, Options}),
	NewS = S#state{limit=N-1, refs=gb_sets:add(Ref, R)},
	{reply, {ok, Pid}, NewS};
%%% When limit for workers reached.
handle_call({search, _Term, _Options}, _From, 
						S=#state{limit=N}) when N =< 0 ->
	{reply, no_alloc, S};
%%% All other calls.
handle_call(Request, _From, State) ->
	io:format("MINER_SERVER: Unknown call: ~p~n", [Request]),
	{reply, {undef_call, Request}, State}.


%% 
%% @doc Starts a worker and attaches it to the worker supervisor responsible.
%%
start_worker() ->
	ChildSpec = {erlang:unique_integer(), {miner_worker, start_link, []},
							temporary, 5000, worker, [miner_worker]},
	supervisor:start_child(miner_worker_sup, ChildSpec).






