# Youtube Data API Requests

HashTux requests to the Youtube Data API are handled in the module `youtube_search.erl`. 

## Setting a Youtube Application
To create a Youtube application, follow the steps listed below:
* create a Google Account to access the Google Developer Console (GDC);
* create a project in the GDC;
* get authorization credentials.

## Authorization Credentials 
Requests to the Youtube Data API must be [authenticated](https://developers.google.com/youtube/registering_an_application) with OAuth 2.0 credentials or by an API Key. OAuth 2.0 requests are intended to access private user data. Since HashTux.com is not focused on user-specific information, our requests use an **API Server Key**.

## Quota-cost and partial resources
To prevent users from affecting the quality and performance of the service, the Youtube Data API applies a cost named [quota-cost](https://developers.google.com/youtube/v3/getting-started) to any request. The quota-cost associated to a single request depends on the *type* of request and the resource *parts* that are requested.

##External Dependencies
* [jsx](https://github.com/talentdeficit/jsx): used to parse a json text (utf8 encoded binary) and convert it into erlang terms.

## Youtube Data API requests logic
HashTux requests to the Youtube API are carried out in two steps:
* first we send a **Search request** using the [Search:list] (https://developers.google.com/youtube/v3/docs/search/list) method to identify the videos that match query parameters specified by the user. This request will return a list of *N* video IDs.
* Second, we send a **Video request** using the [Video:list](https://developers.google.com/youtube/v3/docs/videos/list) method to retrieve all the relevant information on the IDs returned by the Search request. Note that in this context *N* is the number of Video items returned by the Search request.

## To request Youtube Videos
To send a request to the Youtube Data API use `youtube_search:search/2`:
```erlang
youtube_search:search(Keyword, Options).
```
where `Keyword` is the term requested by the user, while `Options` is a list containing the key-value tuples for the **content_type**, **language** and **history_timestamp** requested by the user.

Parameters must be provided according to the following guidelines:

* **content_type:** `{content_type, TypeList}`, where `TypeList` contains the required content-types specified as binary-strings. Note that if the client is not requesting Video feeds, this function will NOT query the API and will return directly the value `[{filtered, []}, {unfiltered, []}]`.

* **language:** `{language, Language}`, where `Language` is the language requested by the user in binary-string format. To retrive Videos available in all langages, `Language` must be provided as an empty list.

* **history_timestamp:** `{history_timestamp, Timestamp}`, where `Timestamp` is the creation date of the Youtube videos that we want to fetch from the Youtube Data API. `Timestamp` must be provided in epoch-timestamp format. If we want to retrieve the most recent Videos without requesting a specific date, `Timestamp` must be provided as an empty list.

Once the API has returned a response, fetched Video resources are decoded and converted in *"internal representation format"*  through the module **parser.erl**. For more information on the specifications of HashTux *"internal representation format"* read **Protocols & DataType specification**.

`youtube_search:search/2` returns a list of two tuples formatted as `[{filtered, FilteredRes}, {unfiltered, LangResult}]`, where:

* **UnfilteredRes** is a list of videos in *"internal representation format"* matching the specified optional value for the history timestamp params;
* **FilteredRes**: the resulting list after filtering UnfilteredRes by the language requested by the user. 
