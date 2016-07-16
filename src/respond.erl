-module(respond).

-export([handler/1]).

-define(CHANCE, 0.80).

handler(Context) ->
    Nick = proplists:get_value(nick, Context),
    Args = proplists:get_value(args, Context),
    From = maps:get(from, Args),
    To = maps:get(to, Args),
    _Msg = maps:get(msg, Args),
    Response = string:join(markov_server:generate(13), " "),
    Target = case To of Nick -> From; Channel -> Channel end,
    case rand:uniform() of
        X when X < ?CHANCE -> nani_bot:say(Target, Response);
        _ -> ok
    end,
    {continue, Context}.