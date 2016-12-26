-module(blackjack_commands).

-behavior(gen_event).

%% API
-export([add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
         terminate/2, code_change/3]).

%% handlers
-export([handle_deal_command/0,
         handle_hit_command/0,
         handle_status_command/0,
         handle_stand_command/0]).

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
        ["bj", "deal"] ->
            H = handle_deal_command,
            MFA = {Mod, H, []},
            apply_command(From, To, MFA);
        ["bj", "hit"] ->
            H = handle_hit_command,
            MFA = {Mod, H, []},
            apply_command(From, To, MFA);
        ["bj", "stand"] ->
            H = handle_stand_command,
            MFA = {Mod, H, []},
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

handle_deal_command() ->
    Res = blackjack:deal(frotz),
    format_result(Res).

handle_hit_command() ->
    Res = blackjack:hit(frotz),
    format_result(Res).

handle_status_command() ->
    Res = blackjack:status(),
    format_result(Res).

handle_stand_command() ->
    Res = blackjack:stand(frotz),
    format_result(Res).
 
format_result({State, {player, PlayerScore, PlayerCards}, {house, HouseScore, HouseCards}}) ->
   PlayerCardStr = string:join([deck:format_card(X) || X <- PlayerCards], " "),
    HouseCardStr = string:join([deck:format_card(X) || X <- HouseCards], " "),
    Args = [State, PlayerScore, PlayerCardStr, HouseScore, HouseCardStr],
    Msg = io_lib:format("[~p] player ~p (~s), house ~p (~s)", Args),
    {ok, Msg};
 
format_result(Thing) ->
    {ok, io_lib:format("~p", [Thing])}.