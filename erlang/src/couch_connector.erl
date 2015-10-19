%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% @doc Initial database actions module
%% %% @version 0.1

-module(couch_connector).

-define(URI, "derkahler.de").
-define(PORT, "1994").
-define(USER, "hashtux").
-define(PW, "grouptux").
-define(ADDR,"http://" ++ ?URI ++ ":" ++ ?PORT ++ "/").

-export([get_info/0, put_request/3, get_request/1, delete_request/1, post_request/3]).
 -version("0.1").

auth_header(User, Pass) ->
	            Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
		                    {"Authorization","Basic " ++ Encoded}.

get_info() ->
	get_request([]).

put_request(DocAddr, Content, Type) ->
	Headers = [auth_header(?USER, ?PW), {"Content-Type", Type}],
	Options = [{body_format, string}],
	httpc:request(put, {?ADDR ++ DocAddr, Headers, Type, Content}, [], Options).

%% Is not completed
post_request(DocAddr, Content, Type) ->
	Headers = [auth_header(?USER, ?PW), {"Content-Type", Type}],
	Options = [{body_format, binary}],
	httpc:request(post, {?ADDR ++ DocAddr, Headers, Type, Content}, [], Options).

get_request(DocAddr) ->
	Headers = [auth_header(?USER, ?PW)],
	Options = [{body_format, binary}],
	httpc:request(get, {?ADDR ++ DocAddr, Headers}, [], Options).

delete_request(DocAddr) ->
	Headers = [auth_header(?USER, ?PW)],
	Options = [{body_format, binary}],
	httpc:request(delete, {?ADDR ++ DocAddr, Headers}, [], Options).
