-module(markov_respond).

-behavior(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
         terminate/2, code_change/3]).

-export([add_handler/0, delete_handler/0]).

init([]) ->
    {ok, []}.

handle_event({privmsg, Nick, _From, To, Text}, State) ->
    % Let's learn some new vocab
    markov_server:seed(Text),

    % TODO: Check for messages to the bot itself (To =:= Nick).
    % Respond when we encounter some text with our own nick
    case re:run(Text, Nick, [global, caseless]) of
        {match, _} -> 
            Tokens = markov_server:generate(13),
            Msg = string:join(Tokens, " "),
            nani_bot:say(To, Msg),
            {ok, State};
        _ -> 
            {ok, State}
    end;

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_handler() ->
    nani_event:add_handler(?MODULE, []).

delete_handler() ->
    nani_event:delete_handler(?MODULE, []).