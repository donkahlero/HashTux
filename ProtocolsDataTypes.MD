#Protocols & DataType specification
##Social Media Post Datatype / Document
This is the format in which we store the social media posts in the database or return them to the client.
We usually pass several posts around at a time. Hence generally the posts need to be wrapped in a list:
```erlang
[[Document1], [Document2], [Document3], [DocumentN]].
```
The format of the document is the following (written in Erlang compatible with PSX JSON):
```erlang
[{<<"search_term">>, <<"The search term without a hashtag">>},
{<<"service_id">>, <<"Post ID in external service">>},
{<<"user_id">>, <<"The User ID in external service">>},
{<<"username">>, <<"@UserName">>},
{<<"display_name">>, <<"The Real Name">>},
{<<"service">>}, <<"twitter or youtube or instagram">>},
{<<"content_type">>, <<"image or video or text">>},
{<<"resource_link_low">>, <<"resource link in low res">>},
{<<"resource_link_high">>, <<"resourch link in high res">>},
{<<"timestamp">>, timestamp_as_integer},
{<<"text">>, <<"The content text">>},
{<<"language">>, <<"the language like en">>},
{<<"location">>, {lat, long, place},
{<<"view_count">>, view_count_as_integer},
{<<"like_count">>, like_count_as_integer},
{<<"tags">>, [The Tags]}]
```

##Format of requests to the Erlang backend
A php file called request.php can be used to generate a properly structured request by first using the build_request_data() function and then making the actual request via the request() function.

Nevertheless, here is the format of the request:
 - Use HTTP POST requests.
 - Supply the search term as the path in the request, for example http://localhost:8080/my_search_term
 - In the request body, provide a JSON list of [options, user_habit_data]. Note that in Erlang the keys in each key-value tuple is converted to an atom, but while in JSON they are still strings.

Options - a JSON object with the following key-value pairs:
 - request_type: "search" for initial search (more results) or "update" for subsequent (ajax) requests.
 - service: a list, any combination of "twitter", "instagram", "youtube" or just [] for all.
 - content_type: a list, any combination of "image", "text", "video" or just [] for all.
 - language: two lowercase characters representation

User Habit Data - a JSON object with the following key-value pairs:
 - timetamp: Unix epoch time, number
 - session_id: PHP session string
 - ip_address: String
 - platform: Platform, free text
 - browser: Web browser, free text
 - browserversion: Web browser version, free text

Example of the whole [options, user_habit_data] request body: 
```json
[{"request_type":"search",
  "services":[],
  "content_type":[],
  "language":"en"},
 {"timestamp":1447435268,
  "session_id":"1t8iv8lt4c54h9ht3gc1ml2997",
  "ip_address":"178.174.190.192",
  "browser":"Firefox",
  "browser_version":"42.0",
  "platform":"Windows"}] 
```

When decoded to erlang by JSX, the option part looks like this
```erlang
[{"request_type", <<"search">>},
  {"services", []},
  {"content_type", []},
  {"language", <<"en">>}]
```
