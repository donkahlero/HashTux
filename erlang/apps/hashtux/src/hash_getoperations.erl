%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% @doc Initial database actions module
%% %% @version 0.1

-module(hash_getoperations).

-export([hash_getHash/1, hash_getCont/1, hash_exist/1]).


%% @doc This gets the information of the hash.
hash_getHash(Hash) ->
	jsx:decode(couch_operations:doc_get(Hash)).

%% @doc This gets the content from a hash without "_id" and "_rev".
hash_getCont(Hash) ->
	[{Field, Val} || {Field, Val} <- hash_getHash(Hash), Field =/= <<"_id">>, Field =/= <<"_rev">>].

%% @doc This checks if a hash exist in the DB.
hash_exist(Hash) ->
	couch_operations:doc_exist(Hash).


