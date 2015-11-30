-module(miner_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).




%% =============================================================================
%% API functions
%% =============================================================================


start_link() ->
	io:format("MINER_SUP: Starting...~n"),
  	supervisor:start_link({local, ?MODULE}, ?MODULE, []).




%% =============================================================================
%% Supervisor callbacks
%% =============================================================================


init([]) ->
	MinerDBWriter = {miner_dbwriter, 
							 {miner_dbwriter, start_link, []},
						 		permanent, 
								10000, 
								worker, 
								[miner_dbwriter]},
	MinerServer = {miner_server, 
							 {miner_server, start_link, []},
						 		permanent, 
								10000, 
								worker, 
								[miner_server]},
	MinerWorkerSup = {miner_worker_sup,
							 {miner_worker_sup, start_link, []},
								permanent,
								10000,
								worker,
								[miner_worker_sup]},
	{ok, { {one_for_one, 3, 1800}, [MinerDBWriter, MinerServer, MinerWorkerSup]} }.





