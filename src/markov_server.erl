-module(markov_server).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([start/0, start_link/0, stop/0, lookup/1, seed/1, seed_file/1, generate/1]).

-define(OBJECTS_TABLE, ngrams_objects).
-define(INDEX_TABLE, ngrams_index).
-define(SERVER, ?MODULE).

-record(state, {index, objects, count}).

%%%============================================================================
%%% API
%%%============================================================================
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

lookup(Key) ->
    gen_server:call(?SERVER, {lookup, Key}).

seed(Text) ->
    gen_server:cast(?SERVER, {seed, Text}).

seed_file(Path) ->
    gen_server:cast(?SERVER, {seed_file, Path}).

generate(Count) ->
    gen_server:call(?SERVER, {generate, Count}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================
init([]) ->
    Index = ets:new(?INDEX_TABLE, []),
    Objects = ets:new(?OBJECTS_TABLE, []),
    State = #state{index = Index, objects = Objects, count = 0},
    {ok, State}.

handle_call({lookup, Key}, _From, State) ->
    Objects = State#state.objects,
    Reply = ets:lookup(Objects, Key),
    {reply, Reply, State};

handle_call({generate, Count}, _From, State) ->
    Reply = generate(Count, State, []),
    {reply, Reply, State}.

handle_cast({seed_file, Path}, State) ->
    {ok, Contents} = file:read_file(Path),
    Text = binary_to_list(Contents),
    NewKeyCount = seed(Text, State), 
    {noreply, State#state{count = NewKeyCount}};

handle_cast({seed, Text}, State) ->
    NewKeyCount = seed(Text, State),
    {noreply, State#state{count = NewKeyCount}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> 
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
random_key(State) ->
    Index = State#state.index,
    KeyCount = State#state.count,
    % TODO: I'm not totally feeling happy about this
    RandomKey = rand:uniform(KeyCount) - 1,
    [Thing] = ets:lookup(Index, RandomKey),
    Thing.
    
random_next(Key, State) ->
    Objects = State#state.objects,
    case ets:lookup(Objects, Key) of
        [] -> [];
        [{_, Candidates}] -> Candidates
    end. 

generate(_Count, State, _Acc) ->
    Thing = random_key(State),
    {_Index, RandomKey} = Thing,
    {RandomKey, random_next(RandomKey, State)}.

seed(Text, State) ->
    Index = State#state.index,
    Objects = State#state.objects,
    Count = State#state.count,
    Grams = analyze(2, string:to_lower(Text)),
    store(Index, Objects, Grams, Count).

analyze(Order, Text) ->
    Tokens = markov:tokenize(Text),
    Grams = markov:ngrams(Order, Tokens),
    markov:analyze(Order, Grams).

store(_Index, _Objects, [], Count) -> Count;

store(Index, Objects, [{Key, Following} | Rest], Count) ->
    Object = case ets:lookup(Objects, Key) of
        [{_Key, Existing}] -> {Key, [Following | Existing]};
        [] -> {Key, [Following]}
    end,
    ets:insert(Objects, Object),
    NewCount = case ets:match_object(Index, {'_', Key}) of
        [_Something] -> Count;
        [] -> ets:insert_new(Index, {Count, Key}), Count + 1
    end,
    store(Index, Objects, Rest, NewCount).