%% @author Jonas Kahler jonas@derkahler.de [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc CouchDB connector. Uses CouchDB's RESTful API. Covers just the most
%% base level operations. Further abstractions are covered in other modules.
%% @version 0.1
%% -----------------------------------------------------------------------------
%% | Sprint 1 // v0.1                                                          |
%% | Added all necessary REST operations to our CouchDB.                       |
%% -----------------------------------------------------------------------------
-module(couch_connector).

%% These are macros describing the DB user, pass and address + port.
-define(URI, "derkahler.de").
-define(PORT, "1994").
-define(USER, "hashtux").
-define(PW, "grouptux").
-define(ADDR,"http://" ++ ?URI ++ ":" ++ ?PORT ++ "/").

-export([get_info/0, put_request/3, get_request/1, delete_request/1]).
 -version("0.1").

%% @doc Function creating a auth header for the HTTP request.
auth_header(User, Pass) ->
	            Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
		                    {"Authorization","Basic " ++ Encoded}.

%% @doc Function fetching the general information from the database.
get_info() ->
	?MODULE:get_request([]).

%% @doc This function puts in a new DB or new document as well as adding fields to it.
put_request(DocAddr, Content, Type) ->
	Headers = [auth_header(?USER, ?PW), {"Content-Type", Type}],
	Options = [{body_format, string}],
	httpc:request(put, {?ADDR ++ DocAddr, Headers, Type, Content}, [], Options).

%% @doc This function gets the infromation from the DB. You can get general information of go deeper
%% and get specific document and json files.
get_request(DocAddr) ->
	Headers = [auth_header(?USER, ?PW)],
	Options = [{body_format, binary}],
	httpc:request(get, {?ADDR ++ DocAddr, Headers}, [], Options).

%% @doc This function deletes data from the DB.
delete_request(DocAddr) ->
	Headers = [auth_header(?USER, ?PW)],
	Options = [{body_format, binary}],
	httpc:request(delete, {?ADDR ++ DocAddr, Headers}, [], Options).
