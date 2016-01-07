%% @author Jerker Ersare <jerker@soundandvision.se>
%% @doc The main flow server starts a new worker to handle each request.
%%
%% The main flow workers are responsible for processing a request correctly once it is
%% received from the HTTP interface, by contacting the DB and miners as approperiate,
%% and then returning a response to the HTTP interface to be returned to the Apache
%% server.
%% 
%% This is based the server-worker structure on Ivos structure for miner server and miner 
%% workers.
%% 
%% (We don't want any trouble occuring on the processes started by Cowboy, that is one
%% of the reasons we choose to use our own server-worker structure for this)

-module(main_flow_server).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0, stop/0]).

%% Keeps track of the supervisors started and their pids
-record(state, {limit=2000, refs, queue=queue:new()}).


%%% ============================================================
%%% PUBLIC API
%%% ============================================================


%% @doc Starts the server
start_link() ->	
	gen_server:start_link({local, main_flow_server}, ?MODULE, [], []).


%% @doc Stops the server
stop() ->
	gen_server:cast(?MODULE, stop).



%%% ============================================================
%%% CALLBACK FUNCTIONS
%%% ============================================================

%% @doc Creates a worker and sends back the worker's PID to the using code.
handle_call({RequestType, Term, Options}, From, 
						S=#state{limit=N, refs=R}) when N > 0 ->
	
	% Start a new worker to handle the request.
	io:format("main_flow_server: Starting worker~n", []),
	{ok, Pid} = start_worker(),
	Ref = erlang:monitor(process, Pid),
	
	% Send a message to the worker through cast.
	% We include a reference to the PID that made the original call,
	% so the worker can reply when finished.
	{SourcePID, _} = From,
	gen_server:cast(Pid, {RequestType, SourcePID, Term, Options}),
	
	% Update the state of the main flow server.
	NewS = S#state{limit=N-1, refs=gb_sets:add(Ref, R)},
	{reply, {ok, Pid}, NewS};

%% When limit of workers is reached, just return immediately.
handle_call({_RequestType, _Term, _Options}, _From, 
						S=#state{limit=N, refs=_R}) when N =< 0 ->	
	{reply, no_alloc, S}.


%% @doc Do nothing.
handle_cast(Msg, State) ->
	{noreply, Msg, State}.	


%% @doc Initialises the server.
init([]) -> 
	{ok, #state{refs=gb_sets:empty()}}.


%% @doc Terminates the server.
terminate(Reason, _State) ->
	io:format("main_flow_server: Stopping, reason:~p~n", [Reason]),
	ok.


%% @doc Code upgrade, not implemented. 
code_change(_PrevVersion, _State, _Extra) ->
	{ok}.


%% @doc Handle info, most relevant is when a child is finished, we will
%% receive a DOWN message - the child should be removed from state queue. 
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


%% @doc Starts a worker and attaches it to the worker supervisor
start_worker() ->
	ChildSpec = {erlang:unique_integer(), {main_flow_worker, start_link, []},
							temporary, 5000, worker, [main_flow_worker]},
	supervisor:start_child(main_flow_worker_sup, ChildSpec).