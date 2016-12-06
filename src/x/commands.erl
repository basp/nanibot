-module(commands).

-export([info/1]).

%%%============================================================================
%%% API
%%%============================================================================
info(Context) ->
    Nick = proplists:get_value(nick, Context),
    Age = proplists:get_value(age, Context),
    Args = proplists:get_value(args, Context),
    From = maps:get(from, Args),
    To = maps:get(to, Args),
    Msg = maps:get(msg, Args),
    Target = case To of Nick -> From; Channel -> Channel end,
    RegexOpts = [{capture, none}, global, caseless],
    case re:run(Msg, "^!info", RegexOpts) of
        match ->
            Data = markov_server:info(),
            Info = [extract_info(T) || T <- Data ],
            nani_bot:say(Target, io_lib:format("~p~n", [Info])),
            {halt, Context};
        nomatch -> 
            {continue, Context}
    end.

%%%============================================================================
%%% Internal functions
%%%============================================================================
extract_info(Info) ->
    {Table, Opts} = Info,
    io:format("~p~n", [Info]),
    Size = proplists:get_value(size, Opts),
    Memory = proplists:get_value(memory, Opts),
    {Table, [{size, Size}, {memory, Memory}]}.
