-module(respond).

-export([handler/1]).

-define(CHANCE, 0.28).

handler(Context) ->
    Nick = proplists:get_value(nick, Context),
    Args = proplists:get_value(args, Context),
    From = maps:get(from, Args),
    To = maps:get(to, Args),
    Msg = maps:get(msg, Args),

    %RegexOpts = [{capture, none}, global, caseless],

    Response = string:join(markov_server:generate(13), " "),
    Target = case To of Nick -> From; Channel -> Channel end,
    case rand:uniform() of
       X when X < ?CHANCE -> 
           nani_bot:say(Target, Response);
       _ -> 
           ok
    end,
    {continue, Context}.

reply_to_alias(Msg, Context) ->
    Response = foo,
    {replied, Response, Context}.

reply_to_question(Question, Context) ->
    Response = foo,
    {replied, Response, Context}.

reply_to_exclamation(Statement, Context) ->
    Response = foo,
    {replied, Response, Context}.

reply_randomly(Msg, Context) ->
    Response = foo,
    {replied, Response, Context}.