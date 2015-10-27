hashtux
=====

##CouchDB operations
* Start the supervisor: db_sup:start_link/0
* Do all the operations with the following gen_server: db_serv - it will send a message with the result back to your mailbox. The call will always return you the reference of the worker to patternmatch later on.

###Example: Get content for Hashtag jerkeriscool
```erlang
db_sup:start_link(). [or on the shell please: db_sup:shell_start().]
Ref = gen_server:call(db_serv, {get_cont, "jerkeriscool"}).
receive {Ref, Res} -> Res end.
```
--> Always pattern match for the reference to the worker to ensure that you receive the correct result

###Supported operations
* {get_hash, Hashtag}
* {get_cont, Hashtag}
* {hash_exist, Hashtag}
* {add_hash, Hashtag, Content}
* {overwr_hash, Hashtag, Content}
* {add_content, Hashtag, Content}
* {remove_val, Hashtag, Field}
* {delete_hash, Hashtag}

####Legend
* Hashtag is a String! => in Erlang a list
* Content is of the JSX Erlang JSON representation:
```JSX
[{<<"name">>, <<"Jerker">>}, {<<"lastname">>, <<"Ericsson">>}]
```
* Field is a String - like "name"
