%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Initial database actions module
%% @version 0.1
-module(couch_operations).

%% DB name for all operations
-define(DB, "hashtux/").

%% All document operations
-export([add_doc/2, change_doc/2, get_doc/1, delete_doc/1]).

%% @doc Method to add a document to the database
add_doc(DocName, Content) ->
    couch_connector:put_request(DocName, Content, "text/json").

%% @doc Change conent of a document
change_doc(DocName, Content) ->
    Rev = jsx:decode(?MODULE:get_doc(DocName)).
    %couch_connector:put_request(DocName, Rev ++ Content, "text/json").

%% @doc Get a document from the database
get_doc(DocName) ->
    {ok, {_HTTP, _Info, Res}} = couch_connector:get_request(?DB ++ DocName),
    Res.

delete_doc(DocName) ->
    Rev = ?MODULE:get_doc(DocName),
    couch_connector:delete_request(DocName ++ "?rev=" ++ Rev).
