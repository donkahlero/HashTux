%%%-------------------------------------------------------------------
%% @author Jerker Ersare <jerker@soundandvision.se>
%% @doc This module starts the cowboy HTTP server and defines 
%% http_handler as it's handler module.
%%%-------------------------------------------------------------------

-module('http_cowboy').

-export([start/0]).

%% @doc Starts the cowboy HTTP server. The handler is defined in http_handler.erl.
start() ->
	%% Check the configuration for which port to listen to
	{ok, Port} = application:get_env(listener_conf, listen_port),

	%% Start the http listener	
	Dispatch = cowboy_router:compile([
        {'_', [{'_', http_handler, []}]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	io:format("~n~nhashtux_app: Started the cowboy http_handler at port ~p.~n~n", [Port]).