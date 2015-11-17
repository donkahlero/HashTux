-module(db_worker_sup).

-behavior(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Supname) ->
        supervisor:start_link({local, Supname}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []} }.
