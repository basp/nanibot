-module(nani_utils).

-export([parse/1]).

parse(<<":", Line/binary>>) ->
    [Prefix | Rest] = re:split(Line, " ", [{parts, 2}]),
    [Nick | _User] = re:split(Prefix, "[!@]", [{parts, 2}]),
    parse_command(Rest, [Nick]);
    
parse(Line) ->
    parse_command(Line, [<<>>, <<>>]).

parse_command(Line, Acc) ->
    [Front | Trailing] = re:split(Line, " :", [{parts, 2}]),
    Parts = case (length(Trailing)) of 0 -> 16; _ -> 15 end,
    [Command | Params] = re:split(Front, " ", [{parts, Parts}]),
    {match, Acc ++ [Command] ++ Params ++ Trailing}.

