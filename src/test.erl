-module(test).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([start_link/0, insert/1, lookup/1]).

-define(TABLE, test).
-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

insert(Object) -> 
    gen_server:cast(?SERVER, {insert, Object}).

lookup(Key) ->
    gen_server:call(?SERVER, {lookup, Key}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([]) -> 
    {ok, ets:new(?TABLE, [])}.

handle_call({lookup, Key}, _From, State) ->
    Reply = ets:lookup(State, Key),
    {reply, Reply, State}.

handle_cast({insert, Object}, State) ->
    ets:insert(State, Object),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functions
%%%============================================================================