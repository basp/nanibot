-module(credits_commands).

-behavior(gen_event).

%% API
-export([add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
         terminate/2, code_change/3]).

%% handlers
-export([handle_credits_command/1]).

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
    Mod = credits_commands,
    case Tokens of
        ["credits"] ->
            H = handle_credits_command,
            MFA = {Mod, H, [From]},
            apply_command(From, To, MFA);
        ["cr"] ->
            H = handle_credits_command,
            MFA = {Mod, H, [From]},
            apply_command(From, To, MFA);
        ["cr", "status"] -> 
            H = handle_credits_command,
            MFA = {Mod, H, [From]},
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

handle_credits_command(Who) ->
    {_, Credits} = credits_server:status(Who),
    Msg = io_lib:format("~p", [Credits]),
    {ok, Msg}.