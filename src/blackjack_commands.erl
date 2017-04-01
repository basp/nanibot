-module(blackjack_commands).

-behavior(gen_event).

%% API
-export([add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
         terminate/2, code_change/3]).

%% handlers
-export([handle_deal_command/1,
         handle_bet_command/2,
         handle_hit_command/1,
         handle_status_command/0,
         handle_stand_command/1]).

%%%============================================================================
%%% API
%%%============================================================================
add_handler() ->
    nani_event:add_handler(?MODULE, []).

delete_handler() ->
    nani_event:delete_handler(?MODULE, []).

%%%============================================================================
%%% gen_event callbacks
%%%============================================================================
init([]) -> {ok, []}.

handle_event({cmd, _Bot, From, To, Cmd}, State) ->
    Tokens = nani_utils:parse_cmd(Cmd),
    Mod = blackjack_commands,
    case Tokens of
        ["bj", "bet", Arg] ->
            H = handle_bet_command,
            MFA = {Mod, H, [Arg, From]},
            apply_command(From, To, MFA);
        ["bj", "deal"] ->
            H = handle_deal_command,
            MFA = {Mod, H, [From]},
            apply_command(From, To, MFA);
        ["bj", "hit"] ->
            H = handle_hit_command,
            MFA = {Mod, H, [From]},
            apply_command(From, To, MFA);
        ["bj", "stand"] ->
            H = handle_stand_command,
            MFA = {Mod, H, [From]},
            apply_command(From, To, MFA);
        ["bj", "status"] ->
            H = handle_status_command,
            MFA = {Mod, H, []},
            apply_command(From, To, MFA);
        _ -> ok
    end,
    {ok, State};

handle_event(_Event, State) -> {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
apply_command(From, To, {M, F, A}) ->
    case apply(M, F, A) of 
        {ok, Str} -> 
            nani_bot:say(To, [From, ": ", Str]);
        Err -> 
            Msg = io_lib:format("~w", [Err]),
            nani_bot:say(To, [From, ": ", Msg])
    end.

handle_bet_command(Arg, Who) ->
    case try_parse_int(Arg) of
        {ok, Amount} when Amount =< 1000 -> 
            Res = blackjack:bet(frotz, Amount),
            handle_result_credits(Who, Res),
            format_result(Res);
        {ok, _} -> {error, maxbet};
        Err -> Err
    end.

handle_deal_command(Who) ->
    Res = blackjack:deal(frotz),
    handle_result_credits(Who, Res),
    format_result(Res).

handle_hit_command(Who) ->
    Res = blackjack:hit(frotz),
    handle_result_credits(Who, Res),
    format_result(Res).

handle_stand_command(Who) ->
    Res = blackjack:stand(frotz),
    handle_result_credits(Who, Res),
    format_result(Res).

handle_status_command() ->
    Res = blackjack:status(),
    format_result(Res).
 
handle_result_credits(Who, Res) ->
    case Res of 
        {player_won, {player, _, _, Bet}, _} -> 
            credits_server:deposit(Who, Bet);
        {house_won, {player, _, _, Bet}, _} ->
            credits_server:withdraw(Who, Bet);
        _ -> ok
    end.

format_result(Res = {State, {player, PlayerScore, PlayerCards, Bet}, {house, HouseScore, HouseCards}}) ->
    PlayerCardStr = string:join([deck:format_card(X) || X <- PlayerCards], " "),
    HouseCardStr = string:join([deck:format_card(X) || X <- HouseCards], " "),
    Args = [PlayerScore, PlayerCardStr, HouseScore, HouseCardStr, Bet],
    case State of
        house_won -> 
            Msg = io_lib:format("Dealer wins! Player ~p (~s), dealer ~p (~s) (lost ~p credits)", Args),
            {ok, Msg};
        player_won ->
            Msg = io_lib:format("Player wins! Player ~p (~s), dealer ~p (~s) (gained ~p credits)", Args),
            {ok, Msg};    
        ok -> 
            Msg = io_lib:format("Player ~p (~s), dealer ~p (~s) (current bet is ~p credits)", Args),
            {ok, Msg};
        _  -> 
            {error, Res}
    end;
 
format_result(Thing) ->
    {ok, io_lib:format("~p", [Thing])}.

try_parse_int(Str) ->
    try list_to_integer(Str) of
        Int -> {ok, Int}
    catch
        error:Err -> {error, Err}
    end.