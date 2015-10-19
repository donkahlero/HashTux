%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% @doc Initial database actions module
%% %% @version 0.1

-module(couch_connector).

-define(URI, "derkahler.de").
-define(PORT, "1994").
-define(USER, "hashtux").
-define(PW, "grouptux").
-define(ADDR,"http://" ++ ?URI ++ ":" ++ ?PORT ++ "/").

-export([get_info/0, put_request/3]).
 -version("0.1").


get_info() ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(get, {?ADDR, []}, [], []),
	Body.

put_request(DocAddr, Content, Type) ->
	Headers = [auth_header(?USER, ?PW), {"Content-Type", Type}],
	Options = [{body_format,binary}],
	httpc:request(put, {?ADDR ++ DocAddr, Headers, Type, Content}, [], Options).

auth_header(User, Pass) ->
	    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
	        {"Authorization","Basic " ++ Encoded}.
