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
`List` is an erlang list containing the services we want to search for in binary format. Services supported are **instagram**, **twitter**, **youtube**.For example, `{service, [<<"instagram">>, <<"twitter">>]}`. To search for all services available, either leave the list empty `[]` or omit this {key, value} pair.
```erlang
{content_type, List}
```
`List` is an erlang list containing the content types we want to search for in binary format. Content types supported are **image**, **video**, **text**. For example, `{content_type, [<<"image>>]}`. To search for all content types available, either leave the list empty `[]` or omit this {key, value} pair.
```erlang
{language, Value}
```
`Value` is an erlang binary denoting the language. Languages supported are **en**, **sv**, **it**, **bg**, **de**, **es**, **fr** and **am** (Aman special :sunglasses:). For example, `{language, <<"en">>}`. To search for all languages available, omit this {key, value} pair.
```erlang
{history_timestamp, Seconds}
```
`Seconds` is an integer specifying the timestamp in seconds. For example, `{history_timestamp, 123456}`. To search for results for a concrete timeframe, include the foremention {key, value} pair. Otherwise, omit this {key, value} pair.




### Sequence of operations when above call executed (for now):
1. A new worker is spawned to search for the given term (hashtag). For now search is general, no options or filters applied.
2. After the search is complete, the worker returns the search results to the original caller.

##To be completed:
- Specify the `Options` format
