%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Initial database actions module
%% @version 0.1
-module(couch_operations).

-export([init/0]).

-version("0.1").

init() ->
    Url = "http://derkahler.de:1994",
    Options = [],
    S = couchbeam:server_connection(Url, Options),
    S.
