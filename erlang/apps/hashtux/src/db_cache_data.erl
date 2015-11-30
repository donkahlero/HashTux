%% @author Jonas Kahler <jonas.kahler@icloud.com> [www.derkahler.de]                                                                                      
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]                                                                            
%% @doc A module to handle the caching of the user statisics.                                                                                   
%% @version 0.1                                                                                                                                           
%% -----------------------------------------------------------------------------                                                                          
%% | Sprint 5                                                                  |                                                                          
%% | Created this module for the purpose of handling the caching of the user   |

%% | statisics so it faster to get the results to the front end. Added so that |

%% | it is looping through the cache_data function and will do it every hour.  |                                                                          
%% ----------------------------------------------------------------------------- 

-module(db_cache_data).
-export([start_link/0]).
-export([cache_data/0, delete_db/0, write/1, get_posts/1, write_addr/1, add_db/0]).

start_link() ->
    {ok, spawn_link(fun() -> cache_data() end)}.

cache_data() ->
    delete_db(),
    add_db(),
    write(["search_term", "browser", "platform", "language", "browser_version", "platform_browser"]),
    timer:sleep(20000),
    cache_data().

delete_db()->
    couch_operations:delete_db({db_addr_serv:main_addr() ++
	 "userstats_cached_data/", db_addr_serv:main_user(), db_addr_serv:main_pass()}).

add_db() ->
    couch_operations:add_db({db_addr_serv:main_addr() ++
         "userstats_cached_data/", db_addr_serv:main_user(), db_addr_serv:main_pass()}).

write([]) -> ok;
write([H| T]) ->
    [Today,Week,Month,Year] = get_posts(H),
    couch_operations:doc_add(write_addr(H ++ "_today"), Today),
    couch_operations:doc_add(write_addr(H ++ "_week"), Week),
    couch_operations:doc_add(write_addr(H ++ "_month"), Month),
    couch_operations:doc_add(write_addr(H ++ "_year"), Year),
    write(T).

get_posts(Addr) ->
    db_reduce:reduce(couch_operations:doc_get({db_addr_serv:main_addr() ++
       "hashtux_userstats/_design/stat/_view/by_" ++ Addr ++ "?descending=true", db_addr_serv:main_user(), db_addr_serv:main_pass()})).

write_addr(Doc) ->
    {db_addr_serv:main_addr() ++
         "userstats_cached_data/" ++ Doc, db_addr_serv:main_user(), db_addr_serv:main_pass()}.
