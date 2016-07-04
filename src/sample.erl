-module(sample).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([start_link/0, insert/1, lookup/1, seed/1]).

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

seed(Text) ->
    gen_server:cast(?SERVER, {seed, Text}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([]) -> 
    {ok, ets:new(?TABLE, [])}.

handle_call({lookup, Key}, _From, State) ->
    Reply = ets:lookup(State, Key),
    {reply, Reply, State}.

handle_cast({seed, Text}, State) ->
    Objects = analyze(2, Text),
    store(State, Objects),
    {noreply, State};

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
analyze(Order, Text) ->
    Tokens = markov:tokenize(Text),
    Grams = markov:ngrams(Order, Tokens),
    markov:analyze(Order, Grams).

store(_Tab, []) -> ok;

store(Tab, [{Key, Following} | Rest]) ->
    Object = case ets:lookup(Tab, Key) of
        [{_Key, Existing}] -> {Key, [Following | Existing]};
        [] -> {Key, [Following]}
    end,
    ets:insert(Tab, Object),
    store(Tab, Rest).