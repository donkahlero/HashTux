# Twitter API Requests
=================================================================
HashTux requests to Twitter API are handled in the module `twitter_search.erl`. HashTux requests to Twitter use the [Twitter Search API] (https://dev.twitter.com/rest/public/search).

## Setting a Twitter Application
The Twitter API accepts authenticated requests sent by Twitter Applications. To create a new app, go to [apps.twitter.com](https://apps.twitter.com) and follow the instructions. To generate authentication keys for your application, follow the instructions provided [here](https://dev.twitter.com/oauth/overview/application-owner-access-tokens).

##External Dependencies
* [erlang-oauth](https://github.com/tim/erlang-oauth/): erlang client used to generate signed request parameters.
* [ibrowse](https://github.com/cmullaparthi/ibrowse): HTTP Erlang client.
* [jsx](https://github.com/cmullaparthi/ibrowse): used to parse a json text (utf8 encoded binary) and convert it into erlang terms.

##To request Twitter statuses
To send a GET request to the Twitter Search API use `twitter_search:search_hashtag/2`:
```erlang
twitter_search:search_hashtag(Keyword, Options).
```
where `Keyword` is the term requested by the user, while `Options` is a list containing the key-value tuples for the **content_type**, **language** and **history_timestamp** requested by the user.

```erlang
Options = [{content_type, TypeList}, {language, Language}, {history_timestamp, Timestamp}]
```

Parameters must be provided according to the following guidelines:

* **content_type:** `{content_type, TypeList}`, where `TypeList` contains the required types of feeds specified as binary-strings. For example, if we want to send a request without filtering results by content-type, TypeList must be provided as `[<<"text">>, <<"image">>, <<"video">>]`.

* **language:** `{language, Language}`, where `Language` is the language requested by the user in binary-string format. To retrive Tweets available in all langages, `Language` must be provided as an empty list.

* **history_timestamp:** `{history_timestamp, Timestamp}`, where `Timestamp` is the creation date of the Twitter feeds we want to fetch from the Twitter API. `Timestamp` must be provided in epoch-timestamp format. If we want to retrieve the most recent Tweets without requesting a specific date, `Timestamp` must be provided as an empty list.

Once the API has returned a response, the *Statuses* list is decoded and converted in *"internal representation format"*  through the module **parser.erl**. For more information on the specifications of HashTux *"internal representation format"* read **Protocols & DataType specification**.

`twitter_search:search_hashtag/2` returns a list of two tuples formatted as `[{filtered, FilteredRes}, {unfiltered, LangResult}]`, where:

* **UnfilteredRes** is a list of tweets in *"internal representation format"* matching the specified values for language and history timestamp params;
* **FilteredRes**: the resulting list after filtering UnfilteredRes by the content_type requested by the user. 
