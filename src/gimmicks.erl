-module(gimmicks).

-behavior(gen_event).

%% API
-export([add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
         terminate/2, code_change/3]).

%% handlers
-export([handle_throw_command/3]).

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
    Mod = gimmicks,
    case Tokens of
        ["g", "throw", Arg1] ->
            H = handle_throw_command,
            MFA = {Mod, H, [To, From, Arg1]},
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
    
handle_throw_command(To, From, Thing) ->
    spawn(fun () -> throw_loop(To, From, Thing, 5) end),
    ok.

throw_loop(To, From, Thing, N) when N =< 1 ->
    Msg = io_lib:format("~s ended up a fair distance away.", [Thing]),
    nani_bot:say(To, [From, ": ", Msg]);

throw_loop(To, From, Thing, N) ->
    % nani_bot:say(To, ["Uummph...!"]),
    timer:sleep(1000),
    throw_loop(To, From, Thing, N - 1).