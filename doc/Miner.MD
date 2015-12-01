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
Term is the term (hashtag) to search for, passed as a string, eg:
```erlang
miner_server:search("mario", Options).
```
Options is a list containing {key, value} pairs
For now the `Options` can be any valid erlang term (eg atom, []). The above function returns `{ok, Pid}` where `Pid` is the process identifier of the spawned worker to do the search. 

### Sequence of operations when above call executed (for now):
1. A new worker is spawned to search for the given term (hashtag). For now search is general, no options or filters applied.
2. After the search is complete, the worker returns the search results to the original caller.

##To be completed:
- Specify the `Options` format
