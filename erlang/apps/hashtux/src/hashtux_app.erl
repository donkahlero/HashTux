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
	%% Start the http listener
	%%
	Dispatch = cowboy_router:compile([
        {'_', [{'_', http_handler, []}]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}],
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
