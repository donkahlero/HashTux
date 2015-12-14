# Youtube Data API Requests
=================================================================
HashTux requests to the Youtube Data API are handled in the module `youtube_search.erl`. HashTux requests to the Youtube API are carried out in two steps:
* first we send a **Search request** using the [Search:list] (https://developers.google.com/youtube/v3/docs/search/list) method to identify the videos that match query parameters specified by the user. This request will return a list of *N* video IDs.
* Second, we send a **Video request** using the [Video:list](https://developers.google.com/youtube/v3/docs/videos/list) method to retrieve all the relevant information on the IDs returned by the Search request. Note that in this context *N* is the number of Video items returned by the Search request.

## Authorization Credentials 
Requests to the Youtube Data API must be [authenticated](https://developers.google.com/youtube/registering_an_application) with OAuth 2.0 credentials or by an API Key. OAuth 2.0 requests are intended to access private user data. Since HashTux.com is not focused on user-specific information, our requests use an **API Server Key**.

## Quota-cost and partial resources
To prevent users from affecting the quality and performance of the service, the Youtube Data API applies a cost named [quota-cost](https://developers.google.com/youtube/v3/getting-started) to any request. The quota-cost associated to a single request depends on the *type* of request and the resource *parts* that are requested.

##External Dependencies
* [jsx](https://github.com/cmullaparthi/ibrowse): used to parse a json text (utf8 encoded binary) and convert it into erlang terms.
