-module(db_serv).
-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public functions
start_link() ->    
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

state() ->
  gen_server:call(?MODULE, state).

%% Server implementation, a.k.a.: callbacks
init([]) ->
    io:format("db_serv started...", []),
    {ok, []}.

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Stop the server
handle_cast(stop, State) ->
    io:format("db_serv stopped...", []),
    {stop, normal, State}.

%% Return message
handle_info(Info, State) ->
    io:format("db_serv retrieved ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
