%%%-------------------------------------------------------------------
%% @doc This module starts the cowboy HTTP server.
%% @end
%%%-------------------------------------------------------------------

-module('http_cowboy').

-export([start/0]).

%% @doc Start the cowboy HTTP server, handler is defined in http_handler.erl.
start() ->
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
	io:format("~n~nhashtux_app: Started the cowboy http_handler.~n~n", []).