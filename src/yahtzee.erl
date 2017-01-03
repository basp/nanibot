-module(yahtzee).

-behaviour(gen_statem).

%% API
-export([start/0,
         start_link/0,
         stop/0,
         bet/2,
         roll/1,
         keep/2,
         status/0]).

%% state functions
-export([standby/3,
         player_roll/3,
         player_keep/3]).

%% gen_statem callbacks
-export([init/1,
         callback_mode/0,
         handle_event/4,
         handle_call/3,
         terminate/3,
         code_change/4]).

-record(state, {results, turn, round, dice, kept, score}).

name() -> yahtzee.

%%%============================================================================
%%% API
%%%============================================================================
start() ->
    gen_statem:start({local, name()}, ?MODULE, [], []).

start_link() ->
    gen_statem:start({local, name()}, ?MODULE, [], []).

bet(Who, Amount) ->
    gen_statem:call(name(), {bet, Who, Amount}).

keep(Who, Wanted) ->
    gen_statem:call(name(), {keep, Who, Wanted}).

roll(Who) ->
    gen_statem:call(name(), {roll, Who}).

status() ->
    gen_statem:call(name(), status).

stop() ->
    gen_statem:stop(name()).


%%%============================================================================
%%% gen_statem callbacks
%%%============================================================================
init([]) -> 
    Data = #state{results = [], turn = 0, round = 0, dice = [], kept = []},
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

standby({call, From}, {bet, _Who, Bet}, Data) ->
    Reply = {ok, {bet, Bet}},
    NewData = Data#state{results = [], turn = 1, round = 1, dice = [1,1,1,1,1], kept = []},
    {next_state, player_roll, NewData, [{reply, From, Reply}]};

standby({call, From}, status, Data) ->
    Reply = {ok, standby},
    {keep_state, Data, [{reply, From, Reply}]};

standby({call, From}, _Msg, Data) ->
    Reply = {ok, standby},
    {keep_state, Data, [{reply, From, Reply}]}.

%%%----------------------------------------------------------------------------
player_roll(cast, _Event, _Data) ->
    {keep_state_and_data, []};

player_roll({call, From}, {roll, _Who}, Data = #state{turn = Turn, round = Round, dice = Dice, results = Results}) when Turn < 3 ->
    Roll = roll_dice(length(Dice)),
    Reply = {ok, Turn, Round, Roll},
    NewData = Data#state{turn = Turn + 1, dice = Roll, results = [Roll|Results]},
    {next_state, player_keep, NewData, [{reply, From, Reply}]};

player_roll({call, From}, {roll, _Who}, Data = #state{turn = Turn, round = Round, dice = Dice, results = Results}) when Round < 1 ->
    Roll = roll_dice(length(Dice)),
    Reply = {ok, Turn, Round, Roll},
    NewData = Data#state{turn = 1, round = Round + 1, dice = Roll, results = [Roll|Results]},
    {next_state, player_keep, NewData, [{reply, From, Reply}]};

player_roll({call, From}, {roll, _Who}, Data = #state{round = Round, dice = Dice, kept = Kept}) ->
    Roll = roll_dice(length(Dice)),
    RoundResults = [{Round, Roll ++ Kept}],
    Reply = {ok, RoundResults},
    NewData = Data#state{turn = 0, round = 0, dice = Roll, results = []},
    {next_state, player_keep, NewData, [{reply, From, Reply}]};

player_roll({call, From}, status, Data) ->
    Reply = {ok, player_roll},
    {keep_state, Data, [{reply, From, Reply}]};

player_roll({call, From}, _Msg, Data) ->
    Reply = {ok, player_roll},
    {keep_state, Data, [{reply, From, Reply}]}.

%%%----------------------------------------------------------------------------
player_keep(cast, _Event, _Data) ->
    {keep_state_and_data, []};

player_keep({call, From}, {keep, _Who, Wanted}, Data) ->
    {Dice, Keep} = keep_dice(Data#state.dice, Wanted),
    Kept = Data#state.kept ++ Keep,
    Reply = {ok, {Dice, Kept}},
    NewData = Data#state{dice = Dice, kept = Kept},
    {next_state, player_roll, NewData, [{reply, From, Reply}]};

player_keep({call, From}, _Msg, Data) ->
    Reply = {ok, player_keep},
    {keep_state, Data, [{reply, From, Reply}]}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
roll_dice(N) -> [rand:uniform(6) || _X <- lists:seq(1, N)].

keep_dice(Available, Wanted) -> keep_dice(Available, Wanted, []).

keep_dice([], _, Acc) -> {[], Acc};
keep_dice(Available, [], Acc) -> {Available, Acc};
keep_dice(Available, [H|T], Acc) ->
    case lists:member(H, Available) of
        true -> keep_dice(lists:delete(H, Available), T, [H|Acc]);
        _ -> keep_dice(Available, T, Acc)
    end.