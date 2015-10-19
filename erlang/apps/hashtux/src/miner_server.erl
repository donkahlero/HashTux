-module(miner_server).

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3,
				handle_info/2, handle_call/3, handle_cast/2]).
-export([start_link/0, stop/0]).
-export([miner_search/2]).


%%% PUBLIC API

%% for now register as local -> change later
%% no arguments passed here to callback function init/1
start_link() -> 
	gen_server:start_link({local, miner_server}, ?MODULE, [], []).


%% for stopping the server - asynchronious call
stop() ->
	gen_server:cast(?MODULE, stop).


%% for requesting a search
miner_search(Term, Options) ->
	gen_server:call(?MODULE, {search, Term, Options}).


%%% CALLBACK FUNCTIONS

%% for initialising the server loop
%% in this case no treatment of data
init([]) -> {ok, []}.

%% for abnormal termination
terminate(Reason, _State) ->
	io:format("server stopping for reason: ~p~n", [Reason]),
	ok.

%% for new version of the code
code_change(_PrevVersion, _State, _Extra) ->
	{ok}.

%% for handling messages sent directly with the
%% ! operator, init/1's timeouts, monitor's 
%% notifications and 'EXIT' signals
handle_info(Msg, State) ->
	io:format("unknown message: ~p~n", [Msg]),
	{noreply, State}.

%% handles asynchronous messages
handle_cast(stop, State) ->
	{stop, normal, State}.	

%% handles synchronous messages
handle_call({search, Term, _Options}, _From, State) ->
	{reply, Term, State}.



