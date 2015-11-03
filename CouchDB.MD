#CouchDB database
##Startup
To startup simply start the database supervisor:
```erlang
db_sup:start_link().
```

##General usage
To send messages to the server simply make gen_server calls to the responsible dispenser serv <b>db_serv</b>.
The call will return you the reference (PID) of the started worker process.
So you can pattern match for the result in your receive.
```erlang
Ref = gen_serv:call(db_serv, {get_posts, "your_hashtag"}),
receive {Ref, Result} -> Result end.
```

##Supported calls
###Read operations
```erlang
{get_posts, "Hashtag"}
```
* "Hashtag" is the Hashtag you want to search for
* Returns all available posts in a list of JSX JSON results
```erlang
{posts_exist, "Hashtag"}
```
* Checks if there are any posts for a specific Hashtag
* Returns a true or false atom

###Write operations
```erlang
{add_doc, Content}
```
* Adds document(s) (which must be in JSX JSON representation) to the database
* Content is a list of JSX JSON Obj. -> [[{<<"search_term">>, <<"jerker">>}, {<<"lastname">>, <<"ericsson">>}], [...]]
* Returns a true or false atom
```erlang
{add_doc, Content, Limit}
```
* Adds document(s) to the database (like the previous). 
* Limit is the maximum amount of entries you want to receive from the database
```erlang
{add_doc, Content, StartTime, EndTime}
```
* Same as above
* StartTime is the first date the data should come from (UNIX epoch time)
* EndTime is the last date the data should come from (UNIX epoch time)

##ToDo
* Support for more specific searches (limit amount of posts, service [Twitter, Insta, YT], timestamp, etc.)
* Add Userdata support
