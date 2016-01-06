%% @author Jerker Ersare <jerker@soundandvision.se>
%% @doc Supervisor for the main flow. See the source files of main_flow_worker
%% and main_flow_server for more comments. 

-module(main_flow_sup).

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


%% @doc Starts the main flow supervisor
start_link() ->
	io:format("main_flow_sup: Started the main flow supervisor.~n"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


%% @doc Initializes the server and the worker supervisor.
init([]) ->	
	MainFlowServChild = {main_flow_server, 
							 {main_flow_server, start_link, []},
						 		permanent, 
								10000, 
								worker, 
								[main_flow_server]},
	MainFlowWorkChild = {main_flow_worker_sup,
							 {main_flow_worker_sup, start_link, []},
								permanent,
								10000,
								worker,
								[main_flow_worker_sup]},
	{ok, { {one_for_one, 3, 1800}, [MainFlowServChild, MainFlowWorkChild]} }.

