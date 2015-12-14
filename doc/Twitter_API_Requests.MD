# Twitter API Requests
=================================================================
##To request Tweets statuses
To send a GET request to the Twitter Search API use `search_hashtag/2`:
```erlang
twitter_search:search_hashtag(Keyword, Options).
```
where `Keyword` is the term requested by the user, while `Options` is a list containing the key-value tuples for the **content_type**, **language** and **history_timestamp** requested by the user.

Parameters must be provided according to the following guidelines:

* **content_type:** `{content_type, TypeList}`, where `TypeList` contains the required types of feeds specified as binary-strings. For example, if we want to send a request without filtering results by content-type, TypeList must be provided as `[<<"text">>, <<"image">>, <<"video">>]`.

* **language:** `{language, Language}`, where `Language` is the language requested by the user in binary-string format. To retrive Tweets available in all langages, `Language` must be provided as an empty list.

* **history_timestamp:** `{history_timestamp, Timestamp}`, where `Timestamp` is the creation date of the Twitter feeds we want to fetch from the Twitter API. `Timestamp` must be provided in epoch-timestamp format. If we want to retrieve the most recent Tweets without requesting a specific date, `Timestamp` must be provided as an empty list.
