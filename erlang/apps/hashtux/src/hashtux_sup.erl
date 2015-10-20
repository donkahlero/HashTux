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
	%% For now, start the HTTP handler here.
	%% Later, we probably will just start a sub-supervisor for each task
	%% (mining, db handling and http handling).
	
	Dispatch = cowboy_router:compile([
        {'_', [{'_', http_handler, []}]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	io:format("~n~nStarted the cowboy http_handler~n~n", []),
    
	{ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
