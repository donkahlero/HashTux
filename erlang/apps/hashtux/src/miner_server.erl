-module(miner_server).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0, stop/0]).
-export([search/2]).

%% keeps track of the supervisors started and their pids
%% also have a queue to know which request to run next
%% the limit is set to 100 -> after that probably request
%% help from the other servers running
%% the ref can be used to monitor any processes started
-record(state, {limit=1000,
				 refs,
				 queue=queue:new()}).
%%% ============================================================
%%% PUBLIC API
%%% ============================================================


%% for now register as local -> change later
%% no arguments passed here to callback function init/1
start_link() ->	
	gen_server:start_link({local, miner_server}, ?MODULE, [], []).


%% for stopping the server - asynchronious call
stop() ->
	gen_server:cast(?MODULE, stop).


%% for requesting a search
search(Term, Options) ->
	gen_server:call(?MODULE, {search, Term, Options}).



%%% ============================================================
%%% CALLBACK FUNCTIONS
%%% ============================================================

%% for initialising the server loop
%% in this case no treatment of data
%%
%% the worker sup is started dynamically here, to do that
%% we send a message to ourselves(the server in this case)
%% this call is handled in handle_info/2, we do this to get
%% a hold of the pid of the worker sup
init([]) -> 
	{ok, #state{refs=gb_sets:empty()}}.


%% for abnormal termination
terminate(Reason, _State) ->
	io:format("STOPPING:miner_server, REASON:~p~n", [Reason]),
	ok.


%% for new version of the code
code_change(_PrevVersion, State, _Extra) ->
	{ok, State}.


%% handles the down message received from the miner worker
%% when it has finished with the task (or not)
handle_info({'DOWN', Ref, process, _Pid, _}, 
						S=#state{limit=N, refs=Refs}) ->
	case gb_sets:is_element(Ref, Refs) of
		true ->
			NewRefs = gb_sets:delete(Ref, Refs),
			NewS = S#state{limit=N+1, refs=NewRefs},
			{noreply, NewS};
		false ->
			{noreply, S}
	end;
% all other messages -> ignored
handle_info(_Msg, S) ->
	{noreply, S}.


%% handles asynchronous messages
handle_cast(Msg, State) ->
	{noreply, Msg, State}.	


%% handles synchronous messages, search request
handle_call({search, Term, Options}, From, 
						S=#state{limit=N, refs=R}) when N > 0 ->
	{ok, Pid} = start_worker(),
	Ref = erlang:monitor(process, Pid),
	gen_server:cast(Pid, {From, Term, Options}),
	NewS = S#state{limit=N-1, refs=gb_sets:add(Ref, R)},
	{reply, {ok, Pid}, NewS};
% when too many workers running
handle_call({search, Term, Options}, _From, 
						S=#state{limit=N}) when N =< 0 ->
	{reply, {no_alloc, {Term, Options}}, S};
% all other calls
handle_call(Request, _From, State) ->
	{reply, {undef_call, Request}, State}.


%% starts a worker and attaches it to the worker supervisor
start_worker() ->
	ChildSpec = {erlang:unique_integer(), {miner_worker, start_link, []},
							temporary, 5000, worker, [miner_worker]},
	supervisor:start_child(miner_worker_sup, ChildSpec).




