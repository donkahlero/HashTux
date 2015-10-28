hashtux
=====

##CouchDB operations
* Start the supervisor: db_sup:start_link/0
* Do all the operations with the following gen_server: db_serv - it will send a message with the result back to your mailbox. The call will always return you the reference of the worker to patternmatch later on.

###Supported operations
* {add_hashtag_doc, Doc}

###Data representation
* Content is of the JSX Erlang JSON representation:
```JSX
[{<<"name">>, <<"Jerker">>}, {<<"lastname">>, <<"Ericsson">>}]
```
