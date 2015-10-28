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
-record(state, {limit=100,
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
code_change(_PrevVersion, _State, _Extra) ->
	{ok}.


%% for handling messages sent directly with the
%% ! operator, init/1's timeouts, monitor's 
%% notifications and 'EXIT' signals
%%
%% when we get the call we send ourselves to start the 
%% worker supervisor we call the top level sup (miner_module_sup)
%% to dynamically add a child to its tree (in this case the worker 
%% sup), then track the pid and add it to the sup reference in our 
%% state record
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
handle_info(_Msg, S) ->
	{noreply, S}.


%% handles asynchronous messages
handle_cast(Msg, State) ->
	{noreply, Msg, State}.	


%% handles synchronous messages
handle_call({search, Term, Options}, From, 
						S=#state{limit=N, refs=R}) when N > 0 ->
	{ok, Pid} = start_worker(),
	Ref = erlang:monitor(process, Pid),
	gen_server:cast(Pid, {From, Term, Options}),
	NewS = S#state{limit=N-1, refs=gb_sets:add(Ref, R)},
	{reply, {ok, Pid}, NewS}.


%% starts a worker and attaches it to the worker supervisor
start_worker() ->
	ChildSpec = {erlang:unique_integer(), {miner_worker, start_link, []},
							temporary, 5000, worker, [miner_worker]},
	supervisor:start_child(miner_worker_sup, ChildSpec).










