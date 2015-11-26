%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Specific operations on the database (mostly document based). Uses the
%% couch_connector module to do them.
%% @version 0.3
%% -----------------------------------------------------------------------------
%% | Sprint 1 // v0.1                                                          |
%% | Added all basic functions to this module.                                 |
%% -----------------------------------------------------------------------------
%% | Sprint 4 // v0.2                                                          |
%% | Applied the new structure to the module.                                  |
%% | The functions get now the DBAddr, User and Pass passed which is then used |
%% | for calling the db_connector module.                                      |
%% -----------------------------------------------------------------------------
%% | Sprint 5 // v0.3                                                          |
%% | Small hotfixes. Checking now if the results can be pattern matched.       |
%% | The document update function works now with the new general design.       |
%% | Applied changes of db_addr_serv.                                          |
%% -----------------------------------------------------------------------------
-module(couch_operations).
-version(0.3).

-export([doc_add/2]).
-export([doc_change/2, doc_get/1, doc_delete/2, doc_append/2]).
-export([doc_rmval/2, doc_exist/1, get_uuid/0]).

%% @doc Method to initially add a document to the database.
%% Encodes the erlang JSon representation to valid JSon.
doc_add({Addr, User, Pass}, Content) ->
    couch_connector:put_request({Addr, User, Pass},
                    jsx:encode(Content), "text/json").

%% @doc Overwrite content of a document.
doc_change(Cred, Content) ->
    case (?MODULE:doc_get(Cred)) of
        [{<<"error">>,<<"not_found">>}, _] ->
            ?MODULE:doc_add(Cred, Content);
        _ ->
            Rev = get_tupp(?MODULE:doc_get(Cred), "_rev"),
            couch_connector:put_request(Cred,
                 binary_to_list(jsx:encode([Rev | Content])), "text/json")
    end.

%% @doc Fetches a document from the database.
%% Returns just the content of this document. HTML header + info are ignored.
doc_get({Addr, User, Pass}) ->
    {ok, {_HTTP, _Info, Res}} = couch_connector:get_request({Addr, User, Pass}),
        jsx:decode(Res).

%% @doc Deletes a document from the database.
doc_delete({Addr, User, Pass}, Rev) ->
    couch_connector:delete_request({Addr ++ "?rev=" ++ Rev, User, Pass}).

%% @doc Appends field(s) to an existing document in the database.
doc_append({Addr, User, Pass}, Content) ->
    Doc = jsx:decode(doc_get({Addr, User, Pass})),
    couch_connector:put_request({Addr, User, Pass},
                    binary_to_list(jsx:encode(Doc ++ Content)), "text/json").

%% @doc Removes a value/field from a document.
doc_rmval({Addr, User, Pass}, Name) ->
    Doc = remove_val(doc_get({Addr, User, Pass}), Name),
    couch_connector:put_request({Addr, User, Pass},
                    binary_to_list(jsx:encode(Doc)), "text/json").

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
    {ok, {_HTTP, _Inf, Res}} =
    couch_connector:get_request({db_addr_serv:main_addr() ++ "_uuids",
                    db_addr_serv:main_user(), db_addr_serv:main_pass()}),
    [{<<"uuids">>, [UUID]}] = jsx:decode(Res),
    binary_to_list(UUID).

doc_exist({Addr, User, Pass}) ->
    case hd(doc_get({Addr, User, Pass})) of
        {<<"error">>, <<"not_found">>} ->
            false;
        _default ->
            true
    end.
