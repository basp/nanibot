-module(markov_server).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([start/0, start_link/0, 
         stop/0, 
         lookup/1, 
         checkpoint/0,
         seed/1, seed_file/1, seed_dir/1, 
         generate/1,
         info/0]).

-define(OBJECTS_TABLE, ngrams_objects).
-define(INDEX_TABLE, ngrams_index).
-define(SERVER, ?MODULE).
-define(DEFAULT_COUNT, 20).

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

info() ->
    gen_server:call(?SERVER, info).

lookup(Key) ->
    gen_server:call(?SERVER, {lookup, Key}).

checkpoint() ->
    gen_server:cast(?SERVER, checkpoint).

seed(Text) ->
    gen_server:cast(?SERVER, {seed, Text}).

seed_file(Path) ->
    gen_server:cast(?SERVER, {seed_file, Path}).

seed_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    seed_dir(Dir, Files).   

%% Assumes we are seeded
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

handle_call(info, _From, State) ->
    Index = ets:info(State#state.index), 
    Objects = ets:info(State#state.objects),
    Reply = [{index, Index}, {objects, Objects}],
    {reply, Reply, State};

handle_call({lookup, Key}, _From, State) ->
    Objects = State#state.objects,
    Reply = ets:lookup(Objects, Key),
    {reply, Reply, State};

handle_call({generate, Count}, _From, State) ->
    NextKey = random_key(State),
    Reply = generate(Count, NextKey, State, []),
    {reply, Reply, State}.

handle_cast(checkpoint, State) ->
    Index = State#state.index,
    Objects = State#state.objects,
    ok = ets:tab2file(Index, "./index.tab"),
    ok = ets:tab2file(Objects, "./objects.tab"),
    {noreply, State};

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

%%%----------------------------------------------------------------------------
seed_dir(_Dir, []) -> ok;

seed_dir(Dir, [File | Rest]) ->
    seed_file(filename:join(Dir, File)),
    seed_dir(Dir, Rest).

%%%----------------------------------------------------------------------------
random_key(State) ->
    Index = State#state.index,
    KeyCount = State#state.count,
    % TODO: I'm not totally feeling happy about this
    RandomIndex = rand:uniform(KeyCount) - 1,
    [{_Index, Key}] = ets:lookup(Index, RandomIndex),
    Key.
    
%%%----------------------------------------------------------------------------
random_next(Key, State) ->
    Objects = State#state.objects,
    case ets:lookup(Objects, Key) of
        [] -> [];
        [{_, Candidates}] -> Candidates
    end. 

%%%----------------------------------------------------------------------------
generate(0, _Key, _State, Acc) -> 
    lists:reverse(Acc);

% NOTE: Only bigrams for now
generate(Count, {_, B} = Key, State, Acc) ->
    case random_next(Key, State) of
        [] -> 
            lists:reverse(Acc);
        [Head | _Rest] -> 
            NextKey = {B, Head},
            generate(Count - 1, NextKey, State, [Head | Acc])
    end.

%%%----------------------------------------------------------------------------
seed(Text, State) ->
    Index = State#state.index,
    Objects = State#state.objects,
    Count = State#state.count,
    Grams = analyze(2, string:to_lower(Text)),
    store(Index, Objects, Grams, Count).

%%%----------------------------------------------------------------------------
analyze(_Order, []) -> [];

analyze(Order, Text) ->
    Tokens = markov:tokenize(Text),
    Grams = markov:ngrams(Order, Tokens),
    markov:analyze(Order, Grams).

%%%----------------------------------------------------------------------------
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