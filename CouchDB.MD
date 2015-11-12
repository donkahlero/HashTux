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
###Read operations (for posts)
```erlang
{posts_exist, "Hashtag"}
```
* Checks if there are any posts for a specific Hashtag
* Returns a true or false atom

```erlang
{get_posts, "Hashtag"}
```
* "Hashtag" is the Hashtag you want to search for
* Returns all available posts in a list of JSX JSON results

```erlang
{get_posts, "Hashtag", [{option1, Value1}, {option2, Value2}, {option3, Value3}, {optionN, ValueN}]}
```
* As above but with search (multiple) options

#### Search options
```erlang
{content_type, [image, text, video]}
```
* The content_type needs to be send as a list with atoms

```erlang
{service, [twitter, instagram, youtube]}
```
* The services takes the different services we have in a list with atoms

```erlang
{language, "en"}
```
* The language is a string

```erlang
{limit, Int}
```
* Gives you back as many result as you specified in the Int

###Read operations (for stats)
```erlang
{get_stats, SearchField, [Options]}
```
* List of the popular hashes (maximum as big as specified)

#### Stats Search Field
* Is always an atom
* The following atoms are possible

```erlang 
get_search_term
```

```erlang
get_browser
```

```erlang
get_platform
```

```erlang
get_language
```

```erlang
get_browser_version
```
```erlang
get_platform_browser
```

#### Stats options
* For getting statistics you can online specify one Option
* Leaving the Option empty you will get the amount of requests of the DB
* The following are supported:

```erlang
{limit, Int}
```

```erlang
{time, StartTime, EndTime}
```

###Write operations
```erlang
{add_doc, Content}
```
* Adds document(s) (which must be in JSX JSON representation) to the database
* Content is a list of JSX JSON Obj. -> [[{<<"search_term">>, <<"jerker">>}, {<<"lastname">>, <<"ericsson">>}], [...]]
* Returns a true or false atom

```erlang
{add_habit_doc, Content}
```
* Adds ONE document to the user habit data database
* Content is a JSX JSON Obj.

##ToDo
* Support for more specific searches (limit amount of posts, service [Twitter, Insta, YT], timestamp, etc.)
* Add Userdata support
