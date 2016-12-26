-module(blackjack).

-behaviour(gen_statem).

%% API
-export([start/0,
         start_link/0,
         stop/0,
         deal/1,
         hit/1,
         stand/1,
         status/0,
         score/1]).

%% state functions
-export([standby/3,
         player_turn/3]).

%% gen_statem callbacks
-export([init/1,
         callback_mode/0,
         handle_event/4,
         handle_call/3,
         terminate/3,
         code_change/4]).

-record(state, {deck, player, house}).

name() -> blackjack.

%%%============================================================================
%%% API
%%%============================================================================
score(Cards) ->
    Aces = count_aces(Cards),
    Base = base_score(Cards),
    total_score(Aces, Base).

start() ->
    gen_statem:start({local, name()}, ?MODULE, [], []).

start_link() ->
    gen_statem:start({local, name()}, ?MODULE, [], []).

status() ->
    gen_statem:call(name(), status).

deal(Who) ->
    gen_statem:call(name(), {deal, Who}).

hit(Who) ->
    gen_statem:call(name(), {hit, Who}).

stand(Who) ->
    gen_statem:call(name(), {stand, Who}).

stop() ->
    gen_statem:stop(name()).

%%%============================================================================
%%% gen_statem callbacks
%%%============================================================================
init([]) -> 
    Data = #state{deck = [], player = [], house = []},
    {ok, standby, Data}.

terminate(_Reason, _State, _Data) -> ok.

code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.

handle_call({call, From}, _Msg, Data) ->
    {keep_state, Data, [{reply, From, ok}]}.

handle_event(_EventType, _EventContent, _State, _Data) ->
    {keep_state_and_data, []}.

callback_mode() -> state_functions.

%%%============================================================================
%%% State functions
%%%============================================================================

%%%----------------------------------------------------------------------------
standby(cast, _Event, _Data) ->
    {keep_state_and_data, []};

standby({call, From}, {deal, _Who}, Data) ->
    Deck0 = deck:shuffle(deck:french()),
    {Pcard1, Deck1} = deck:draw(Deck0),
    {Hcard1, Deck2} = deck:draw(Deck1),
    {Pcard2, Deck3} = deck:draw(Deck2),
    Player = [Pcard1, Pcard2],
    House = [Hcard1],
    PlayerScore = score(Player),
    HouseScore = score(House),
    case PlayerScore of
        21 -> 
            Reply = {player_won, {player, PlayerScore, Player}, {house, HouseScore, House}},
            {keep_state_and_data, [{reply, From, Reply}]};
        _ ->
            Reply = {ok, {player, PlayerScore, Player}, {house, HouseScore, House}},
            NewData = Data#state{deck = Deck3, player = Player, house = House},
            {next_state, player_turn, NewData, [{reply, From, Reply}]}
    end;

standby({call, From}, status, Data) ->
    Reply = {ok, standby},
    {keep_state, Data, [{reply, From, Reply}]};

standby({call, From}, _Msg, Data) ->
    Reply = {ok, standby},
    {keep_state, Data, [{reply, From, Reply}]}.

%%%----------------------------------------------------------------------------
player_turn(cast, _Event, _Data) ->
    {keep_state_and_data, []};

player_turn({call, From}, {stand, _Who}, Data) ->
    Player = Data#state.player,
    PlayerScore = score(Player),
    Deck = Data#state.deck,
    {TempHouse, Deck1} = draw_to(PlayerScore, Deck, Data#state.house),
    House = lists:reverse(TempHouse),
    NewData = Data#state{deck = Deck1, house = House},
    HouseScore = score(House),
    Reply = case {PlayerScore, HouseScore} of
        {N, N} -> 
            {house_won, {player, PlayerScore, Player}, {house, HouseScore, House}};
        {X, Y} when X < Y andalso Y =< 21 ->
            {house_won, {player, PlayerScore, Player}, {house, HouseScore, House}};
        _ -> 
            {player_won, {player, PlayerScore, Player}, {house, HouseScore, House}}
    end,
    {next_state, standby, NewData, [{reply, From, Reply}]};

player_turn({call, From}, {hit, _Who}, Data) ->
    {Pcard, Deck1} = deck:draw(Data#state.deck),
    Player = Data#state.player ++ [Pcard],
    House = lists:reverse(Data#state.house),
    PlayerScore = score(Player),
    HouseScore = score(House),
    NewData = Data#state{deck = Deck1, player = Player},
    case PlayerScore of
        21 -> 
            Reply = {player_won, {player, PlayerScore, Player}, {house, HouseScore, House}},
            {next_state, standby, NewData, [{reply, From, Reply}]};
        N when N > 21 -> 
            Reply = {house_won, {player, PlayerScore, Player}, {house, HouseScore, House}},
            {next_state, standby, NewData, [{reply, From, Reply}]};
        _ ->
            Reply = {ok, {player, PlayerScore, Player}, {house, HouseScore, House}},
            {keep_state, NewData, [{reply, From, Reply}]}
    end;

player_turn({call, From}, status, Data) ->
    Reply = {ok, player_turn},
    {keep_state, Data, [{reply, From, Reply}]};

player_turn({call, From}, _Msg, Data) ->
    {keep_state, Data, [{reply, From, {ok, player_turn}}]}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
draw_to(_Target, [], Acc) -> {Acc, []};
draw_to(Target, [H|T], Acc) ->
    Cards = [H|Acc],
    Score = score(Cards),
    case Score of
        N when N >= Target -> {Cards, T};
        _ -> draw_to(Target, T, Cards)
    end.  

base_score(Cards) -> base_score(Cards, 0).

base_score([], Acc) -> Acc;
base_score([{Rank, _}|T], Acc) -> base_score(T, Acc + card_score(Rank)).

count_aces(Cards) -> count_aces(Cards, 0).

count_aces([], Acc) -> Acc;
count_aces([{ace, _}|T], Acc) -> count_aces(T, Acc + 1);
count_aces([_|T], Acc) -> count_aces(T, Acc).

total_score(0, Acc) -> Acc;
total_score(_, Acc) when Acc + 10 > 21 -> Acc;
total_score(N, Acc) -> total_score(N - 1, Acc + 10).

card_score(ace) -> 1;
card_score(king) -> 10;
card_score(queen) -> 10;
card_score(jack) -> 10;
card_score(N) -> N.