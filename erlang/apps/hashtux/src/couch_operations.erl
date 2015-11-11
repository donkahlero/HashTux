%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Initial database actions module
%% @version 0.1
-module(couch_operations).


%% Document operations
-export([doc_add/2, doc_get_map_cont/1, doc_get_mapreduce_cont/1, doc_change/2, doc_get/1, doc_delete/2, doc_append/2, doc_rmval/2, doc_exist/1]).
-export([get_uuid/0]).

%% @doc Method to initially add a document to the database.
%% Encodes the erlang JSon representation to valid JSon.
doc_add(Addr, Content) ->
    couch_connector:put_request(Addr, jsx:encode(Content), "text/json").

%% @doc Overwrite content of a document.
doc_change(Addr, Content) ->
    Rev = get_tupp(jsx:decode(?MODULE:doc_get(Addr)), "_rev"),
    couch_connector:put_request(Addr, binary_to_list(jsx:encode([Rev | Content])), "text/json").

%% @doc Fetches a document from the database.
%% Returns just the content of this document. HTML header + info are ignored.
doc_get(Addr) ->
    {ok, {_HTTP, _Info, Res}} = couch_connector:get_request(Addr),
    jsx:decode(Res).

%% @doc Gets just objects from the database without anything else.
doc_get_map_cont(Addr) ->
    Doc = doc_get(Addr),
    [_RowCount, _OffSet | [{_, Rows} | _]] = Doc,
    [Post || [{<<"_id">>, _ID}, {<<"_rev">>, _Rev} | Post] <- [Posts || {<<"value">>, Posts} <- lists:flatten(Rows)]].

doc_get_mapreduce_cont(Addr) ->
	Doc = doc_get(Addr),
	[{_, List}] = Doc,
	List.

%% @doc Deletes a document from the database.
doc_delete(Addr, Rev) ->
    couch_connector:delete_request(Addr ++ "?rev=" ++ Rev).

%% @doc Appends field(s) to an existing document in the database.
doc_append(Addr, Content) ->
    Doc = jsx:decode(doc_get(Addr)),
    couch_connector:put_request(Addr, binary_to_list(jsx:encode(Doc ++ Content)), "text/json").

%% @doc Removes a value/field from a document.
doc_rmval(Addr, Name) ->
    Doc = remove_val(doc_get(Addr), Name),
    couch_connector:put_request(Addr, binary_to_list(jsx:encode(Doc)), "text/json").

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

%% @doc Function getting a UUID from the CouchDB
get_uuid() ->
    {ok, {_HTTP, _Info, Res}} = couch_connector:get_request("_uuids"),
    [{<<"uuids">>, [UUID]}] = jsx:decode(Res),
    binary_to_list(UUID).

doc_exist(Addr) ->
	case hd(doc_get(Addr)) of
		{<<"error">>, <<"not_found">>} -> false;
		_default -> true
	end.
