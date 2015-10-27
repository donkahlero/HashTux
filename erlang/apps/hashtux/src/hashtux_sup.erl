%%%-------------------------------------------------------------------
%% @doc hashtux top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('hashtux_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	io:format("Started the top level supervisor.~n~n"),
	%%
	%% The cowboy HTTP handler is already set up.
	%%
	%% Here we can start supervisors responsible for the sub-task
	%% data fetching, DB servers and main program flow
	
	%% Start the DB supervisor
	db_sup:start_link(),
	
	%% Start the main flow supervisor
	main_flow_sup:start_link(),
	
	{ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
