%%%-------------------------------------------------------------------
%% @doc hashtux public API
%% @end
%%%-------------------------------------------------------------------

-module('hashtux_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	%%
	%% Bring up the Cowboy HTTP listener. Handler is in http_handler.erl.
	%%

	%% Check the configuration for which port to listen to
	{ok, ListenerConf} = application:get_env(hashtux, instagram_account),
	Port = aux_functions:get_value(listen_port, ListenerConf),

	%% Start the http listener	
	Dispatch = cowboy_router:compile([
        {'_', [{'_', http_handler, []}]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	io:format("~n~nhashtux_app: Started the cowboy http_handler.~n~n", []),
	
	%%
	%% Start the main supervisor
	%%
    'hashtux_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
