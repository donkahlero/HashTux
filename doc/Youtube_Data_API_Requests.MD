# Youtube Data API Requests
=================================================================
HashTux requests to the Youtube Data API are handled in the module `youtube_search.erl`. HashTux requests to the Youtube API are carried out in two steps:
* first we send a **Search request** using the [Search:list] (https://developers.google.com/youtube/v3/docs/search/list) method to identify the videos that match query parameters specified by the user. This request will return a list of *N* video IDs.
* Second, we send *N* **Video requests** request using the [Video:list](https://developers.google.com/youtube/v3/docs/videos/list) method to retrieve the information we need to display in our front-end. Note that in this context *N* is the number of Video items returned by the Search request.

##External Dependencies
* [erlang-oauth](https://github.com/tim/erlang-oauth/): erlang client used to generate signed request parameters.
* [ibrowse](https://github.com/cmullaparthi/ibrowse): HTTP Erlang client.
* [jsx](https://github.com/cmullaparthi/ibrowse): used to parse a json text (utf8 encoded binary) and convert it into erlang terms.
