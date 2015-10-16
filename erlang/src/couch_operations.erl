%% @author Jonas Kahler <jonas@derkahler.de> [www.derkahler.de]
%% @author Niklas le Comte niklas.lecomte@hotmail.com [www.hashtux.com/niklas]
%% @doc Initial database actions module
%% @version 0.1
-module(couch_operations).

-version("0.1").

-behavior(gen_server).

-export([init/1]).
-export([start/0, stop/0, state/0]).
-export([get_hashtag/0]).
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% General public functions
start() ->    
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

state() ->
  gen_server:state(?MODULE, state).

%% Hashtag functions
get_hashtag() ->
    gen_server:call(?MODULE, getht).

%% Server function
init([]) ->
      {ok, 0}.

handle_call(getht, _From, State) ->
    {reply, "That works!", State + 1};
handle_call(stop, _From, State) ->    
    {stop, normal, stopped, State + 1};
handle_call(state, _From, State) ->
    {reply, State, State + 1};
handle_call(_Request, _From, State) ->
    {reply, ok, State + 1}.

handle_cast(_Msg, State) ->
    {noreply, State + 1}.

handle_info(_Info, State) ->
    {noreply, State + 1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
