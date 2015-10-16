%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% %% @doc Initial database actions module
%% %% @version 0.1

 -module(couch_connector).

-export([start_link/0, stop/0]).
-export([get_info/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
 -version("0.1").

-behavior(gen_server).

%%Normal exp
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	        stop(?MODULE).
stop(Module) ->
	        gen_server:call(Module, stop).

%%API for users
get_info() ->
	        gen_server:call(?MODULE, info).

%%Server stuff
init(_Args) ->
	URI = "http://www.derkahler.de:1994/",
	{ok, {URI}}.

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};

handle_call(info, _From, {URI}) ->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(get, {URI, []}, [], []),
	{reply, Body, {URI}}.

handle_cast(_Msg, State) ->
    {noreply, State + 1}.

handle_info(_Info, State) ->
    {noreply, State + 1}.

code_change(_OldVsn, State, _Extra) ->
	    {ok, State}.

terminate(Reason, State) ->
	{exit, Reason, State}.

