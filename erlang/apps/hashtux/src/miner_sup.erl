-module(miner_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	io:format("~nMINER_SUP: Starting...~n"),
  	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	ServChild = {miner_server, 
							 {miner_server, start_link, []},
						 		permanent, 
								10000, 
								worker, 
								[miner_server]},
	WorkChild = {miner_worker_sup,
							 {miner_worker_sup, start_link, []},
								permanent,
								10000,
								worker,
								[miner_worker_sup]},
	{ok, { {one_for_one, 3, 1800}, [ServChild, WorkChild]} }.

