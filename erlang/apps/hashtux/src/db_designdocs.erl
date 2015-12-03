-module(db_designdocs).

-export([get_post/0, get_stat/0, get_filter/0]).

get_post() ->
  [
   {<<"_id">>, <<"_design/post">>},
   {<<"language">>, <<"javascript">>},
   {<<"views">>,
    [{<<"all">>,
      [{<<"map">>, <<"function(doc) { if (doc.search_term)  emit(null, doc) }">>}]},
    {<<"by_hashtag">>,
      [{<<"map">>, <<"function(doc) { if (doc.search_term)  emit(doc.search_term, doc) }">>}]},
    {<<"by_hashtag_date">>,
      [{<<"map">>, <<"function(doc) { if (doc.search_term && doc.timestamp)  emit([doc.search_term, doc.timestamp], doc) }">>}]},
    {<<"by_service">>,
      [{<<"map">>, <<"function(doc) { if (doc.search_term && doc.social_media) emit([doc_search_term, doc.social_media], doc) }">>}]},
    {<<"by_insert_timestamp">>,
      [{<<"map">>, <<"function(doc) { if (doc.insert_timestamp) emit(doc.insert_timestamp, doc) }">>}]}
    ]}
  ].

get_stat() ->
    [
     {<<"_id">>, <<"_design/stat">>},
     {<<"language">>, <<"javascript">>},
     {<<"views">>,
      [{<<"all">>,
        [{<<"map">>, <<"function(doc) { emit(null, doc) }">>}]},
       {<<"by_search_term">>,
        [{<<"map">>, <<"function(doc) { if (doc.term && doc.timestamp)  emit(doc.timestamp, doc.term) }">>}]},
       {<<"by_browser">>,
        [{<<"map">>, <<"function(doc) { if (doc.browser && doc.timestamp)  emit(doc.timestamp, doc.browser) }">>}]},
       {<<"by_language">>,
        [{<<"map">>, <<"function(doc) { if (doc.language && doc.timestamp)  emit(doc.timestamp, doc.language) }">>}]},
       {<<"by_platform">>,
        [{<<"map">>, <<"function(doc) { if (doc.platform && doc.timestamp)  emit(doc.timestamp, doc.platform) }">>}]},
       {<<"by_browser_version">>,
        [{<<"map">>, <<"function(doc) { if (doc.browser && doc.browser_version && doc.timestamp)  emit(doc.timestamp, [doc.browser, doc.browser_version]) }">>}]},
       {<<"by_platform_browser">>,
        [{<<"map">>, <<"function(doc) { if (doc.browser && doc.platform && doc.timestamp)  emit(doc.timestamp, [doc.platform, doc.browser]) }">>}]}
      ]}
    ].

get_filter() ->
    [
     {<<"_id">>, <<"_design/filters">>},
     {<<"filters">>,
      [{<<"deletedfilter">>, <<"function(doc, req) { return !doc._deleted; };">>}]}
   ].
