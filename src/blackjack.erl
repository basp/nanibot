-module(blackjack).

-behaviour(gen_statem).

%% API
-export([start/0,
         start_link/0,
         stop/0,
         deal/1,
         bet/2,
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

-record(state, {deck, player, house, bet}).

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
    gen_statem:call(name(), {bet, Who, 0}).

bet(Who, Amount) ->
    gen_statem:call(name(), {bet, Who, Amount}).

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
    Data = #state{deck = [], player = [], house = [], bet = 0},
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
    PlayerInfo = {player, PlayerScore, Player, 0},
    HouseInfo = {house, HouseScore, House},
    case PlayerScore of
        21 -> 
            Reply = {player_won, PlayerInfo, HouseInfo},
            {keep_state_and_data, [{reply, From, Reply}]};
        _ ->
            Reply = {ok, PlayerInfo, HouseInfo},
            NewData = Data#state{deck = Deck3, player = Player, house = House},
            {next_state, player_turn, NewData, [{reply, From, Reply}]}
    end;

standby({call, From}, {bet, _Who, Bet}, Data) ->
    Deck0 = deck:shuffle(deck:french()),
    {Pcard1, Deck1} = deck:draw(Deck0),
    {Hcard1, Deck2} = deck:draw(Deck1),
    {Pcard2, Deck3} = deck:draw(Deck2),
    Player = [Pcard1, Pcard2],
    House = [Hcard1],
    PlayerData = {player, score(Player), Player, Bet},
    HouseData = {house, score(House), House},
    case PlayerData of
        {player, 21, _Cards, Bet} -> 
            Reply = {player_won, PlayerData, HouseData},
            NewData = Data#state{deck = Deck3, player = Player, house = House, bet = Bet},
            {keep_state, NewData, [{reply, From, Reply}]};
        {player, _, _Cards, Bet} ->
            Reply = {ok, PlayerData, HouseData},
            NewData = Data#state{deck = Deck3, player = Player, house = House, bet = Bet},
            {next_state, player_turn, NewData, [{reply, From, Reply}]}
    end;

standby({call, From}, status, Data) ->
    Bet = Data#state.bet,
    Reply = {ok, standby, {bet, Bet}},
    {keep_state, Data, [{reply, From, Reply}]};

standby({call, From}, _Msg, Data) ->
    Bet = Data#state.bet,
    Reply = {ok, standby, {bet, Bet}},
    {keep_state, Data, [{reply, From, Reply}]}.

%%%----------------------------------------------------------------------------
player_turn(cast, _Event, _Data) ->
    {keep_state_and_data, []};

player_turn({call, From}, {stand, _Who}, Data) ->
    Bet = Data#state.bet,
    Player = Data#state.player,
    PlayerScore = score(Player),
    Deck = Data#state.deck,
    {TempHouse, Deck1} = draw_to(PlayerScore, Deck, Data#state.house),
    House = lists:reverse(TempHouse),
    NewData = Data#state{deck = Deck1, house = House},
    HouseScore = score(House),
    PlayerInfo = {player, PlayerScore, Player, Bet},
    HouseInfo = {house, HouseScore, House},
    Reply = case {PlayerScore, HouseScore} of
        {N, N} -> 
            % It's a draw, we'll decide on `house_won` just because
            {house_won, PlayerInfo, HouseInfo};
        {X, Y} when X < Y andalso Y =< 21 ->
            {house_won, PlayerInfo, HouseInfo};
        _ -> 
            {player_won, PlayerInfo, HouseInfo}
    end,
    {next_state, standby, NewData, [{reply, From, Reply}]};

player_turn({call, From}, {hit, _Who}, Data) ->
    Bet = Data#state.bet,
    {Pcard, Deck1} = deck:draw(Data#state.deck),
    Player = Data#state.player ++ [Pcard],
    House = lists:reverse(Data#state.house),
    PlayerScore = score(Player),
    HouseScore = score(House),
    NewData = Data#state{deck = Deck1, player = Player},
    PlayerInfo = {player, PlayerScore, Player, Bet},
    HouseInfo = {house, HouseScore, House},
    case PlayerScore of
        21 -> 
            Reply = {player_won, PlayerInfo, HouseInfo},
            {next_state, standby, NewData, [{reply, From, Reply}]};
        N when N > 21 -> 
            Reply = {house_won, PlayerInfo, HouseInfo},
            {next_state, standby, NewData, [{reply, From, Reply}]};
        _ ->
            Reply = {ok, PlayerInfo, HouseInfo},
            {keep_state, NewData, [{reply, From, Reply}]}
    end;

player_turn({call, From}, status, Data) ->
    Bet = Data#state.bet,
    Reply = {ok, player_turn, {bet, Bet}},
    {keep_state, Data, [{reply, From, Reply}]};

player_turn({call, From}, _Msg, Data) ->
    Bet = Data#state.bet,
    Reply = {ok, player_turn, {bet, Bet}},
    {keep_state, Data, [{reply, From, Reply}]}.

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