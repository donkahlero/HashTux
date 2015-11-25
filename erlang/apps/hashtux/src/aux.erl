-module(aux).

-export([bin_to_atom/1, get_value/2, get_youtube_keys/0, get_twitter_keys/0]).


bin_to_atom(Binary) ->
	list_to_atom(binary_to_list(Binary)).

%%
%% @doc Helper function to retrieve key values
%%
get_value(_Key, [])	  -> [];
get_value(_Key, null) -> [];
get_value(Key, List)  ->
	case lists:keyfind(Key, 1, List) of
		{_K, V}	-> V;
		false 	-> []
	end.

%%
%% @doc Gets the Youtube Data API 'SERVER KEY'
%%
get_youtube_keys() ->
	{ok, Account} = application:get_env(hashtux, youtube_account),
	get_value(server_key, Account). 

%%
%% @doc Gets the Twitter API Keys
%%
get_twitter_keys() ->
	{ok, Account} = application:get_env(hashtux, twitter_account),
	AccessToken = get_value(access_token, Account),
	AccessTokenSecret = get_value(access_token_secret, Account),
	ConsumerKey = get_value(consumer_key, Account),
	ConsumerKeySecret = get_value(consumer_key_secret, Account),
	Keys = {AccessToken, AccessTokenSecret, ConsumerKey, ConsumerKeySecret},
	Keys.