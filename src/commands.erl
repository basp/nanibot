-module(commands).

-behavior(gen_event).

%% API
-export([add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
         terminate/2, code_change/3]).

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
    Tokens = parse_cmd(Cmd),
    Mod = commands,
    case Tokens of
        ["fac", Arg] -> 
            H = handle_fac_command,
            MFA = {Mod, H, [Arg]},
            apply_command(From, To, MFA);
        ["fmt", Fmt, Arg] ->
            H = handle_fmt_command,
            MFA = {Mod, H, [Fmt, Arg]},
            apply_command(From, To, MFA);
        ["fread", Fmt, Arg] ->
            H = handle_fread_command,
            MFA = {Mod, H, [Fmt, Arg]},
            apply_command(From, To, MFA);
        ["roll"] ->
            H = handle_roll_command,
            MFA = {Mod, H, ["1", "6"]},
            apply_command(From, To, MFA);
        ["roll", NumSides] ->
            H = handle_roll_command,
            MFA = {Mod, H, ["1", NumSides]},
            apply_command(From, To, MFA);
        ["roll", NumDice, NumSides] ->
            H = handle_roll_command,
            MFA = {Mod, H, [NumDice, NumSides]},
            apply_command(From, To, MFA);
        _ -> 
            unknown_command(From, To, Cmd)
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
unknown_command(From, To, Cmd) ->
    nani_bot:say(To, [From, ": ", "unknown command '", Cmd, "'"]).

apply_command(From, To, {M, F, A}) ->
    case apply(M, F, A) of 
        {ok, Str} -> 
            nani_bot:say(To, [From, ": ", Str]);
        Err -> 
            Msg = io_lib:format("~p", [Err]),
            nani_bot:say(To, [From, ": ", Msg])
    end.

handle_roll_command(Arg1, Arg2) ->
    case try_parse_int(Arg1) of
        {ok, Dice} ->
            case try_parse_int(Arg2) of
                {ok, Sides} when Dice < 101 andalso Sides < 101 ->
                    Roll = fun(_) -> rand:uniform(Sides) end,
                    Rolls = lists:map(Roll, lists:seq(1, Dice)),
                    Total = lists:sum(Rolls),
                    RollStr = string:join(lists:map(fun integer_to_list/1, Rolls), ", "),
                    {ok, io_lib:format("~p ~s", [Total, "[" ++ RollStr ++ "]"])};
                {ok, _Sides} ->
                    {error, floodgate};
                Err -> Err
            end;
        Err -> Err
    end. 

handle_fac_command(Arg) ->
    case try_parse_int(Arg) of
        {ok, Int} when Int < 101 -> F = fac(Int), {ok, integer_to_list(F)};
        {ok, _Int} -> {error, floodgate};
        Err -> Err
    end. 

handle_fmt_command(Fmt, Arg) ->
    case try_parse_int(Arg) of
        {ok, Int} -> try_fmt_arg(Fmt, Int);
        Err -> Err
    end.

handle_fread_command(Fmt, Arg) ->
    case try_fread_arg(Fmt, Arg) of
        {ok, Val} -> {ok, integer_to_list(Val)};
        Err -> Err
    end.

try_fread_arg(Fmt, Arg) ->
    case io_lib:fread(Fmt, Arg) of
        {ok, [Val], _} -> {ok, Val};
        Err -> Err
    end.

try_fmt_arg(Fmt, Arg) ->
    try io_lib:format(Fmt, [Arg]) of
        Str -> {ok, Str}
    catch
        error:Err -> {error, Err}
    end.

try_parse_int(Str) ->
    try list_to_integer(Str) of
        Int -> {ok, Int}
    catch
        error:Err -> {error, Err}
    end.

fac(0) -> 1;
fac(N) -> N * fac(N - 1).

parse_cmd(Cmd) ->
   Str = binary_to_list(Cmd),
   lists:map(fun string:to_lower/1, string:tokens(Str, " ")).