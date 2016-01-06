%% @author Jerker Ersare <jerker@soundandvision.se>
%% @doc Supervisor for the main flow workers. See the source files of main_flow_worker
%% and main_flow_server for more comments. 


-module(main_flow_worker_sup).

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


%%@doc Starts the main flow woker supervisor 
start_link() ->
	io:format("main_flow_worker_sup: Started main flow worker supervisor.~n"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


%% @doc Init. Just defines the restart behaviour. Children will be created
%% dynamically later.
init([]) ->
	{ok, {{one_for_one, 5, 5000}, []}}.

