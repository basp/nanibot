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
%%% gen_statem callbacks
%%%============================================================================
init([]) -> {ok, []}.

handle_event({cmd, _Bot, From, To, Cmd}, State) ->
    Str = binary_to_list(Cmd),
    Tokens = lists:map(fun string:to_lower/1, string:tokens(Str, " ")),
    case Tokens of
        ["fac", Arg] -> 
            handle_fac_command(From, To, Arg);
        ["fmt", Fmt, Arg] ->
            handle_fmt_command(From, To, Fmt, Arg);
        ["fread", Fmt, Arg] ->
            handle_fread_command(From, To, Fmt, Arg);
        _ -> void
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
handle_fac_command(From, To, Arg) ->
    case try_parse_int(Arg) of
        {ok, Int} -> 
            F = fac(Int),
            nani_bot:say(To, [From, ": ", integer_to_list(F)]);
        {error, _} -> 
            nani_bot:say(To, [From, ": ", Arg, " is not a valid integer."])
    end.

handle_fmt_command(From, To, Fmt, Arg) ->
    case try_parse_int(Arg) of
        {ok, Int} ->
            case try_fmt_arg(Fmt, Int) of
                {ok, Str} ->
                    nani_bot:say(To, [From, ": ", Str]);
                {error, _} ->
                    nani_bot:say(To, [From, ": ", Fmt, " is not a valid format string."])
            end;
        {error, _} ->
            nani_bot:say(To, [From, ": ", Arg, " is not a valid integer."])
    end.

handle_fread_command(From, To, Fmt, Arg) ->
    case try_fread_arg(Fmt, Arg) of
        {ok, Val} ->
            nani_bot:say(To, [From, ": ", integer_to_list(Val)]);
        {error, _} ->
            nani_bot:say(To, [From, ": ", " either the format string or argument doesn't make sense."])
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