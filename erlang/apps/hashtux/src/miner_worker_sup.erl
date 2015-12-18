%%
%% @author Ivo Vryashkov
%%
%% @doc Miner worker supervisor module. Responsible for the miner workers
%% performing searches.
%%
-module(miner_worker_sup).

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


%% 
%% @doc Starts the supervisor.
%%
start_link() ->
	io:format("MINER_WORKER_SUP: Starting...~n"),
  	supervisor:start_link({local, ?MODULE}, ?MODULE, []).




%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


%%
%% @doc Initialises the restart strategy for this supervisor. Since children
%% will be dynamically attached to it, no children spefications are declared.
%%
init([]) ->
	{ok, {{one_for_one, 5, 5000}, []}}.





