#Protocols & DataType specification
##Standart Document
The standart document needs to be wrapped by a list. This is to ensure that we can pass several objects around our application:
```erlang
[[Document1], [Document2], [Document3], [DocumentN]].
```
The format of the document is the following (written in PSX JSON):
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
