-module(db_filter).

-export([content_type/2]).

content_type(L, ContentType) ->
    content_type(L, ContentType, []).

content_type([], _ContentType, Res) ->
    Res;
content_type([X|Xs], ContentType, Res) ->
    BinType = binary:list_to_bin(ContentType),
    Type = lists:keyfind(<<"content_type">>, 1, X),
    case (Type) of
	BinType ->
	    content_type(Xs, ContentType, [X|Res]);
	_ -> content_type(Xs, ContentType, Res)
    end.
