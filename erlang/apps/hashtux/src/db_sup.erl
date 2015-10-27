%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Main OTP supervisor for the CouchDB database part of our application. 
%% @version 0.2
%% -----------------------------------------------------------------------------
%% | Sprint 1 // v0.1                                                          |
%% | Supervisor starts three children: Hashtag reader and writer supervisor as |
%% | our db_serv gen_server worker, which will wait for requests to the db.    |
%% -----------------------------------------------------------------------------
%% | Sprint 2 // v0.2                                                          |
%% | Supervisor starts now sub supervisor for the statistic writer workers.    |
%% -----------------------------------------------------------------------------
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
    HashReadSup = {db_hash_read_sup, {db_hash_read_sup, start_link, []},
		   temporary, 5000, supervisor, [db_hash_read_sup]},
    HashWriteSup = {db_hash_write_sup, {db_hash_write_sup, start_link, []},
		    temporary, 5000, supervisor, [db_hash_write_sup]},
    StatsWriteSup = {db_stats_write_sup, {db_stats_write_sup, start_link, []},
                    temporary, 5000, supervisor, [db_stats_write_sup]},
    DBServ = {db_serv, {db_serv, start_link, []},
	      permanent, 5000, worker, [db_serv]},
    {ok, {{one_for_one, 1, 10}, [HashReadSup, HashWriteSup, StatsWriteSup, DBServ]}}.
