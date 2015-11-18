#General design decisions and mechanisms
##Caching
If the user has made a search and leaves the browser open, the browser will send "heartbeats" preceeding each "update" request (see Request Types section below). <br /><br />
The "heartbeat" requests will trigger the erlang backend to update the cache for the selected search term and options. <br /><br />
When an "update" or "search" request is then made (within the cache time window, currently 60 seconds), the erlang backend will notice that cached data exists for the provided search term and options, and reply with this cached data. If the backend tried to find something but got nothing as a result, it puts a placeholder object in the database so that it's clear that "nothing" (or []) is an approperiate, up to date result to send back.

##Request type
Specified when making a HTTP request to the erlang backend (See document on datatypes and protocols).
- heartbeat: See above.
- search: Used for initial search.
- update: Used for subsequent (ajax) fetching, if the user keeps the browser open.
- stats: Used to request statistical user habit data.  

##Workload Distribution and Availability
###DNS
Our DNS solution means that a connecting client's web browser will connect to one of our available web servers. This in itself provides a mechanism for workload distribution. It also provides a mechanism for increased availability since if a web server goes down, the clients will connect to the web servers that are still running.
###Web application
Our web applications PHP code, through which all codes to the Erlang backend is made (even AJAX calls), will notice a timeout if the local or otherwise preferred Erlang http interface does not respond in time. It will then connect to an alternative Erlang node specified in its configuration.
###Erlang
(Ivo please write here)
###Database
(Jonas please write here)
