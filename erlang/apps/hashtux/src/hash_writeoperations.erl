
%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% %% @doc Initial database actions module
%% %% %% @version 0.1

 -module(hash_writeoperations).

 -export([hash_addHash/2, hash_overwrHash/2, hash_addCont/2, hash_rmval/2, hash_delete/1]).


%% @doc This adds a new Hash to the DB with the content.
hash_addHash(Hash, Content) ->
       	couch_operations:doc_add(Hash, Content).

%% @doc This overwrite a Hash to all the new content
hash_overwrHash(Hash, Content) ->
	 couch_operations:doc_change(Hash, Content).


%% @doc This adds content to a Hash.
hash_addCont(Hash, Content) ->
	couch_operations:doc_append(Hash, Content).


%% @doc This removes a Field from the Hash.
hash_rmval(Hash, Field) ->
	couch_operations:doc_rmval(Hash, Field).


%% @doc This deletes a hash from the DB.
hash_delete(Hash) ->
	couch_operations:doc_delete(Hash).
