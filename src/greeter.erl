-module(greeter).

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

handle_event({names, {Nick, _Alts}, Channel, Names}, State) ->
    Others = lists:filter(fun (X) -> X =/= Nick end, Names),
    Msg = case Others of
            [Someone] -> "Hiya " ++ Someone ++ "!";
            _ -> "Hi all!"
        end,
    nani_bot:say(Channel, Msg),
    {ok, State};

handle_event({join, {Nick, _Alts}, Channel, User}, State) ->
    case Nick =/= User of
        true -> 
            nani_bot:say(Channel, "Hiya " ++ User ++ "!"),
            {ok, State};
        _ -> 
            {ok, State}
    end;

handle_event(_Event, State) -> {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.