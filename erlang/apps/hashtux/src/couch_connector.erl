%% @author Jonas Kahler jonas@derkahler.de [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc CouchDB connector. Uses CouchDB's RESTful API. Covers just the most
%% base level operations. Further abstractions are covered in other modules.
%% @version 0.2
%% -----------------------------------------------------------------------------
%% | Sprint 1 // v0.1                                                          |
%% | Added all necessary REST operations to our CouchDB.                       |
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v0.2                                                          |
%% | Added a post_request to be used for compressing the DB                    |
%% | Requests to the database are more genereal now.                           |
%% | -> Address needs to be passed                                             |
%% -----------------------------------------------------------------------------
-module(couch_connector).
-version(0.2).

-export([get_info/0]).
-export([put_request/3, get_request/1, delete_request/1, post_request/3]).

%% @doc Function creating an http auth header
auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

%% @doc Function fetching the general information from the database.
get_info() ->
    ?MODULE:get_request([]).

%% @doc This function puts in a new DB or new document.
put_request({Addr, User, Pass}, Content, Type) ->
    Headers = [auth_header(User, Pass), {"Content-Type", Type}],
    Options = [{body_format, string}],
    httpc:request(put, {Addr, Headers, Type, Content}, [], Options).

%% @doc This function fetches data from the database.
get_request({Addr, User, Pass}) ->
    Headers = [auth_header(User, Pass)],
    Options = [{body_format, binary}],
    httpc:request(get, {Addr, Headers}, [], Options).

%% @doc This function deletes data from the DB.
delete_request({Addr, User, Pass}) ->
    Headers = [auth_header(User, Pass)],
    Options = [{body_format, binary}],
    httpc:request(delete, {Addr, Headers}, [], Options).

%% @doc This function makes a POST req on the databse.
%% This is mainly used for executing JSON formated Javascript code on the db.
post_request({Addr, User, Pass}, Content, Type) ->
    Headers = [auth_header(User, Pass), {"Content-Type", Type}],
    Options = [{body_format, string}],
    httpc:request(post, {Addr, Headers, Type, Content}, [], Options).
