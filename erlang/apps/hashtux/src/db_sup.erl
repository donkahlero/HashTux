-module(db_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1, shell_start/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

shell_start() ->
    {ok, Pid} = start_link(),
    unlink(Pid).

init([]) ->
    ReadSup = {db_read_sup, {db_read_sup, start_link, []},
	      temporary, 5000, supervisor, [db_read_sup]},
    WriteSup = {db_write_sup, {db_write_sup, start_link, []},
	      temporary, 5000, supervisor, [db_write_sup]},
    DBServ = {db_serv, {db_serv, start_link, []},
	      permanent, 5000, worker, [db_serv]},
    {ok, {{one_for_one, 1, 10}, [ReadSup, WriteSup, DBServ]}}.
