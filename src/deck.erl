-module(deck).

-export([french/0, shuffle/1, draw/1]).

%%%============================================================================
%%% API
%%%============================================================================
french() ->
    Ranks = [2,3,4,5,6,7,8,9,10,jack,queen,king,ace],
    Suits = [clubs,diamonds,hearts,spades],
    [{R, S} || S <- Suits, R <- Ranks].

shuffle(List) ->
    randomize(round(math:log(length(List)) + 0.5), List).

draw([]) -> {none, []};
draw([H|T]) -> {H, T}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
randomize(1, List) -> randomize(List);

randomize(T, List) ->
    F = fun(_E, Acc) -> randomize(Acc) end,
    lists:foldl(F, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) -> {rand:uniform(), A} end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.