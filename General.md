#General design decisions and mechanisms
##Caching
If the user has made a search and leaves the browser open, the browser will send "heartbeats" preceeding each "update" request (see Request Types section below). 
The "heartbeat" requests will trigger the erlang backend to update the cache for the selected search term and options. 
When an "update" or "search" request is then made (within the cache time window, currently 60 seconds), the erlang backend will notice that cached data exists for the provided search term and options, and reply with this cached data. If the backend tried to find something but got nothing as a result, they put a placeholder object in the database so that the Erlang backend is aware that "nothing" (or []) is an approperiate, up to date result to send back.

##Request type
Specified when making a HTTP request to the erlang backend (See document on datatypes and protocols).
- heartbeat: See above.
- search: Used for initial search.
- update: Used for subsequent (ajax) fetching, if the user keeps the browser open.
- stats: Used to request statistical user habit data.  

##Workload Distribution
TOOD. 
