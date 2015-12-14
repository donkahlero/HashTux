# Twitter API Requests
=================================================================
##To request Tweets statuses
To send a GET request to the Twitter Search API use `search_hashtag/2`:
```erlang
twitter_search:search_hashtag(Keyword, Options).
```
where Keyword is the term requested by the user, while `Options` is a list containing the key-value tuples for the **content_type**, **language** and **history_timestamp** requested by the user.
Parameters must be provided according to the following guidelines:
**Content Type:** `{content_type, TypeList}`, where `TypeList` contains the required types of feeds specified as binary-strings. For example, if we want to send a request without specifying content-type filters, TypeList must be provided as `[<<"text">>, <<"image">>, <<"video">>]}`.
