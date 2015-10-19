%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Initial database actions module
%% @version 0.1
-module(couch_operations).

%% DB name for all operations
-define(DB, "hashtux/").

%% All document operations
-export([doc_add/2, doc_change/2, doc_get/1, doc_delete/1, doc_append/2]).

%% @doc Method to add a document to the database
doc_add(DocName, Content) ->
    couch_connector:put_request(?DB ++ DocName, jsx:encode(Content), "text/json").

%% @doc Change conent of a document
doc_change(DocName, Content) ->
    Rev = get_tupp(jsx:decode((?MODULE:get_doc(DocName))), "_rev"),
    io:format("~p", [binary_to_list(jsx:encode([Rev | Content]))]),
    couch_connector:put_request(?DB ++ DocName, binary_to_list(jsx:encode([Rev | Content])), "text/json").

%% @doc Get a document from the database
doc_get(DocName) ->
    {ok, {_HTTP, _Info, Res}} = couch_connector:get_request(?DB ++ DocName),
    Res.

%% @doc Delete document from database
doc_delete(DocName) ->
    Rev = get_val(jsx:decode((?MODULE:get_doc(DocName))), "_rev"),
    couch_connector:delete_request(?DB ++ DocName ++ "?rev=" ++ Rev).

%% @doc Insert data to a document
doc_append(DocName, Content) ->
    Origin = jsx:decode(doc_get(DocName)),
    Rev = get_tupp(Origin, "_rev"),
    Modified = remove_val(Origin, "_rev"),
    couch_connector:put_request(?DB ++ DocName, binary_to_list(jsx:encode([Rev] ++ Modified ++ Content)), "text/json").
%% @doc Removes value from document
remove_val(Origin, Name) ->
    remove_val(Origin, [] , Name).

%% Creates new list without the data that was removed
remove_val([], Res, _Name) ->
    Res;
remove_val([H|T], Res, Name) ->
    {Id, _Val} = H,
    case (binary_to_list(Id)) of
	Name ->
	    remove_val(T, Res, Name);
	_Other ->
	    remove_val(T, [H | Res], Name)
    end.
	    
%% @doc Gets a tuple with Json file
get_tupp([H|T], Field) ->
    {Id, _Val} = H,
    case (binary_to_list(Id)) of
	Field ->
	    H;
	_Other ->
	    get_tupp(T, Field)
    end.

%% @doc Gets the value from the json with that field
get_val(Json, Field) ->
    {_ID, Val} = get_tupp(Json ,Field),
    binary_to_list(Val).
