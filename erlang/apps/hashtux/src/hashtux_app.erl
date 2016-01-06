%%%-------------------------------------------------------------------
%% @author Jerker Ersare <jerker@soundandvision.se>
%% @doc This is the entry point of the application.
%%%-------------------------------------------------------------------

-module('hashtux_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================


%% @doc Start Hashtux
start(_StartType, _StartArgs) ->
	%%
	%% Bring up the Cowboy HTTP listener.
	%%
	http_cowboy:start(),
	
	%%
	%% Start the main supervisor
	%%
    'hashtux_sup':start_link().


%% @doc Stop Hashtux - currently no particular action taken!
%% This could be a good place to bring down Cowboy gracefully and maybe
%% some other subcomponent that needs it, but since we don't keep a lot
%% of state throughout the application we haven't yet seen the need,
%% we haven't had any problems occuring due to too brutal shutdowns.
stop(_State) ->
    ok.