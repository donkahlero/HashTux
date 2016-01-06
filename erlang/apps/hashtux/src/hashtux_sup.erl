%%%-------------------------------------------------------------------
%% @author Jerker Ersare <jerker@soundandvision.se>
%% @doc This is the top level supervisor.
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

%% @doc Start the top level supervisor
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @doc The init function starts three supervisors responsible for
%% distinct areas of the application.
init([]) ->
	io:format("hashtux_sup: Started the top level supervisor! Welcome to hashtux.~n~n"),
	%%
	%% The cowboy HTTP handler is already set up in hashtux_app.erl.
	%%
	%% Here we can start supervisors responsible for the sub-tasks
	%% database connectivity, API mining and main program flow		
	DB_sup = {db_sup, 
				 {db_sup, start_link, []},
			 		permanent, 
					10000, 
					worker, 
					[db_sup]},
	Miner_sup = {miner_sup, 
				 {miner_sup, start_link, []},
			 		permanent, 
					10000, 
					worker, 
					[miner_sup]},
	Main_flow_sup = {main_flow_sup, 
				 {main_flow_sup, start_link, []},
			 		permanent, 
					10000, 
					worker, 
					[main_flow_sup]},
	
	% One for one: "If a child process terminates, only that process is restarted."
	% Intensity 1, period 1: allow children to fail once every second before terminating
	% the top level supervisor.  
	{ok, { {one_for_one, 1, 1}, [DB_sup, Miner_sup, Main_flow_sup]}}.