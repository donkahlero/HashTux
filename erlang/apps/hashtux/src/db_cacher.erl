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

-module(db_cacher).
-export([start_link/0]).
-export([cache_data/0, delete_db/0, write/1, get_posts/3, write_addr/1]).
-export([ add_db/0, db_exist/0, get_posts_list/1]).

% @doc Starts a process which will do the function cache_data()
start_link() ->
    io:format("db_cacher: started...~n", []),
    {ok, spawn_link(fun() -> cache_data() end)}.

% @doc This will loop through and update the cached data for the userstats
cache_data() ->
    Posts = get_posts_list(["search_term", "browser", "platform",
                            "browser_version", "platform_browser"]),
    case db_exist() of
        true -> delete_db();
        false -> ok
    end,
    add_db(),
    write(Posts),
    timer:sleep(3600000),
    cache_data().

% @doc This deletes the whole database
delete_db()->
    couch_operations:delete_db({db_addr_serv:main_addr() ++
          "hashtux_userstats_cached_data/", db_addr_serv:main_user(),
          db_addr_serv:main_pass()}).

% @doc This creates the database
add_db() ->
    couch_operations:add_db({db_addr_serv:main_addr() ++
         "hashtux_userstats_cached_data/", db_addr_serv:main_user(),
         db_addr_serv:main_pass()}).

% @doc This writes to the database and goes through all the different
%  results from get_posts_list and adds them to the database
write([]) -> ok;
write([H| T]) ->
    [By, Today, Week, Month, Year] = H,
    couch_operations:doc_add(write_addr(By ++ "_today"), Today),
    couch_operations:doc_add(write_addr(By ++ "_week"), Week),
    couch_operations:doc_add(write_addr(By ++ "_month"), Month),
    couch_operations:doc_add(write_addr(By ++ "_year"), Year),
    write(T).

get_posts_list(L) ->
    get_posts_list(L, []).
% @doc This function gets the different results from each user habit data
% that we want from  different time periods and stores it in a list of 
% lists.
get_posts_list([], L) -> L;
get_posts_list([H| T], L) ->
    Today = get_posts(H, time(today), time(now)),
    Week = get_posts(H, time(week), time(now)),
    Month = get_posts(H, time(month), time(now)),
    Year = get_posts(H, time(year), time(now)),

    get_posts_list(T, L ++ [[H, Today, Week, Month, Year]]).

% @doc Gets the result from the map funciton in couch for the given 
% startkey and endkey
get_posts(Addr, Start, End) ->
    URL = "hashtux_userstats/_design/stat/_view/by_" ++ Addr ++
	  "?startkey=" ++ integer_to_list(Start) ++  "&endkey=" ++
	  integer_to_list(End),
    db_reduce:reduce(couch_operations:doc_get({db_addr_serv:main_addr() ++
       URL,
       db_addr_serv:main_user(), db_addr_serv:main_pass()})).

% @doc Checks if the database exist
db_exist() ->
    couch_operations:doc_exist({db_addr_serv:main_addr() ++
         "hashtux_userstats_cached_data/",
         db_addr_serv:main_user(), db_addr_serv:main_pass()}).

% @doc Helper function for the url to write to the database
write_addr(Doc) ->
    {db_addr_serv:main_addr() ++
         "hashtux_userstats_cached_data/" ++ Doc, db_addr_serv:main_user(),
         db_addr_serv:main_pass()}.

% @doc Different timestamp from the current time to get the time from
% now, 24 hours ago, week ago, month ago, and year ago.
time(now) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(os:timestamp()))-719528*24*3600;
time(today) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(os:timestamp()))-719528*24*3600-86400;
time(week) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(os:timestamp()))-719528*24*3600-604800;
time(month) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(os:timestamp()))-719528*24*3600-2592000;
time(year) ->
    calendar:datetime_to_gregorian_seconds(
      calendar:now_to_universal_time(os:timestamp()))-719528*24*3600-31104000.
