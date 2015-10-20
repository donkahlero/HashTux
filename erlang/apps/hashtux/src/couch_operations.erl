%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Initial database actions module
%% @version 0.1
-module(couch_operations).

%% Define DB name for all operations
-define(DB, "hashtux/").

%% Document operations
-export([doc_add/2, doc_change/2, doc_get/1, doc_delete/1, doc_append/2, doc_rmval/2]).

%% @doc Method to initially add a document to the database.
%% Encodes the erlang JSon representation to valid JSon.
doc_add(DocName, Content) ->
    couch_connector:put_request(?DB ++ DocName, jsx:encode(Content), "text/json").

%% @doc Overwrite content of a document.
doc_change(DocName, Content) ->
    Rev = get_tupp(jsx:decode(?MODULE:get_doc(DocName)), "_rev"),
    couch_connector:put_request(?DB ++ DocName, binary_to_list(jsx:encode([Rev | Content])), "text/json").

%% @doc Fetches a document from the database.
%% Returns just the content of this document. HTML header + info are ignored.
doc_get(DocName) ->
    {ok, {_HTTP, _Info, Res}} = couch_connector:get_request(?DB ++ DocName),
    Res.

%% @doc Deletes a document from the database.
doc_delete(DocName) ->
    Rev = get_val(jsx:decode((?MODULE:get_doc(DocName))), "_rev"),
    couch_connector:delete_request(?DB ++ DocName ++ "?rev=" ++ Rev).

%% @doc Appends field(s) to an existing document in the database.
doc_append(DocName, Content) ->
    Doc = jsx:decode(doc_get(DocName)),
    couch_connector:put_request(?DB ++ DocName, binary_to_list(jsx:encode(Doc ++ Content)), "text/json").

%% @doc Removes a value/field from a document.
doc_rmval(DocName, Name) ->
    Doc = remove_val(jsx:decode(doc_get(DocName)), Name),
    couch_connector:put_request(?DB ++ DocName, binary_to_list(jsx:encode(Doc)), "text/json").

%% @doc Helperfunction to remove a value from a JSon document.
remove_val(Origin, Name) ->
    [{Id, Val} || {Id, Val} <- Origin, Id =/= binary:list_to_bin(Name)].
	    
%% @doc Gets the value of a JSon field from the JSon.
get_tupp(L, Field) ->
    lists:keyfind(binary:list_to_bin(Field), 1, L).

%% @doc Extracts just the value from the get_tupp/2 function. 
get_val(Json, Field) ->
    {_ID, Val} = get_tupp(Json, Field),
    binary_to_list(Val).
