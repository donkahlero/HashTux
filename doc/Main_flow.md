# Main flow
=================================================================
(maybe this file is totally unccessesary.... very thin right now anyway)
(maybe this file is totally unccessesary.... very thin right now anyway)
(maybe this file is totally unccessesary.... very thin right now anyway)

##To start the main flow server/worker structure
Start its main supervisor:
```erlang
main_flow_sup:start_link(),
```

##Requests
Make a gen server call in the format 
```erlang
{RequestType, Term, Options}
```
See the general documentation file and protocols and datatype documentation for further explanation of the arguments.
###Search and update requests
These first query the cache, then the miners if needed
###Heartbeat requests
These trigger the miners to cache data
###Stats requests
These query the DB for statistical user habit data.
