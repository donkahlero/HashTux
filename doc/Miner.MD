# Miners
=================================================================
##To start the entire tree for the miner (supervisors + workers)
```erlang
miner_sup:start_link().
```

##To search for a hashtag
```erlang
miner_server:search(Term, Options).
```
The above function returns `{ok, Pid}` where `Pid` is the process identifier of the spawned worker to do the search. 


Term is the term (hashtag) to search for, passed as a string, eg:
```erlang
miner_server:search("mario", Options).
```
Options is a list containing {key, value} pairs. Key is an `atom` and value is a `binary`. Following {key, value} pairs are supported:
```erlang
{service, List}
```
`List` is an erlang list containing the services we want to search for in binary format. For example, `{service, [<<"instagram">>, <<"twitter">>]}`. To search for all services available, either leave the list empty `[]` or omit this {key, value} pair.



### Sequence of operations when above call executed (for now):
1. A new worker is spawned to search for the given term (hashtag). For now search is general, no options or filters applied.
2. After the search is complete, the worker returns the search results to the original caller.

##To be completed:
- Specify the `Options` format
