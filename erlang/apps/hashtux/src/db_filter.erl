-module(db_filter).

-export([content_type/2]).

content_type(L, CTypes) ->
    content_type(L, CTypes, []).

content_type([], _CTypes, Res) ->
    Res;
content_type([X|Xs], CTypes, Res) ->
    {_, Type} = lists:keyfind(<<"content_type">>, 1, X),
    case (is_ctype(CTypes, Type)) of
	false ->
	    content_type(Xs, CTypes, Res);
	true ->
	    content_type(Xs, CTypes, [X|Res])
    end.
    
is_ctype([], _Type) ->
    false;
is_ctype([X|Xs], Type) ->
    CType = binary:list_to_bin(atom_to_list(X)),
    case(Type) of
	CType -> true;
	_ -> is_ctype(Xs, Type)
    end.
