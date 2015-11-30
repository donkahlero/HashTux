-module(db_cache_data).
-export([start_link/0]).
-export([cach_data/0, delete_db/0, write/1, get_posts/1, write_addr/1, add_db/0]).

%% db_addr_serv:main_addr().
%% db_addr_serv:main_user().
%% db_addr_serv:main_pass().

start_link() ->
    {ok, spawn_link(fun() -> cach_data() end)}.

cach_data() ->
    delete_db(),
    add_db(),
    write(["search_term", "browser", "platform", "language", "browser_version", "platform_browser"]),
    timer:sleep(60000),
    cach_data().

delete_db()->
    couch_operations:delete_db({db_addr_serv:main_addr() ++
	 "userstats_cached_data/", db_addr_serv:main_user(), db_addr_serv:main_pass()}).

add_db() ->
    couch_operations:add_db({db_addr_serv:main_addr() ++
         "userstats_cached_data/", db_addr_serv:main_user(), db_addr_serv:main_pass()}).

write([]) -> ok;
write([H| T]) ->
    Posts = get_posts(H),
    couch_operations:doc_add(write_addr(H ++ "_today"), Posts),
    couch_operations:doc_add(write_addr(H ++ "_week"), Posts),
    couch_operations:doc_add(write_addr(H ++ "_month"), Posts),
    couch_operations:doc_add(write_addr(H ++ "_year"), Posts),
    write(T).

get_posts(Addr) ->
    couch_operations:doc_get({db_addr_serv:main_addr() ++
       "hashtux_userstats/_design/stat/_view/by_" ++ Addr ++ "?descending=true", db_addr_serv:main_user(), db_addr_serv:main_pass()}).

write_addr(Doc) ->
    {db_addr_serv:main_addr() ++
         "userstats_cached_data/" ++ Doc, db_addr_serv:main_user(), db_addr_serv:main_pass()}.
