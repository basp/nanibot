-module(markov).

%% API
-export([tokenize/1, ngrams/2, bigrams/1, trigrams/1, analyze/2]).

-define(SEPARATORS, " \t\r\n~!@#$%^&*-_+=()[]{}\\|;:\",.<>?/").

-spec tokenize(string()) -> [string()].
-spec ngrams(integer(), [string()]) -> [tuple()].
-spec analyze(integer(), [tuple()]) -> [{tuple(), string()}]. 
-spec bigrams([string()]) -> [{string(), string()}].
-spec trigrams([string()]) -> [{string(), string(), string()}].

%%%============================================================================
%%% API
%%%============================================================================
tokenize(Text) -> 
    string:tokens(Text, ?SEPARATORS).

ngrams(_Order, [_Something]) -> [];

ngrams(_order, [_Some, _Another]) -> [];

ngrams(Order, Tokens) ->
    ngrams(Order, Tokens, []).

analyze(Order, Grams) ->
    analyze(Order, Grams, []).

bigrams(Tokens) ->
    ngrams(2, Tokens).

trigrams(Tokens) ->
    ngrams(3, Tokens).

%%%============================================================================
%%% Internal functions
%%%============================================================================

%%%----------------------------------------------------------------------------
ngrams(2, [], Acc) ->
    lists:reverse(Acc);

ngrams(2, [A, B], Acc) ->
    Gram = {A, B},
    ngrams(2, [], [Gram | Acc]);

ngrams(2, [A, B | Rest], Acc) ->
    Gram = {A, B},
    ngrams(2, [B | Rest], [Gram | Acc]);

%%%----------------------------------------------------------------------------
ngrams(3, [], Acc) ->
    lists:reverse(Acc);

ngrams(3, [A, B, C], Acc) ->
    Gram = {A, B, C},
    ngrams(3, [], [Gram | Acc]);

ngrams(3, [A, B, C | Rest], Acc) ->
    Gram = {A, B, C},
    ngrams(3, [B, C | Rest], [Gram | Acc]);

%%%----------------------------------------------------------------------------
ngrams(4, [], Acc) ->
    lists:reverse(Acc);

ngrams(4, [A, B, C, D], Acc) ->
    Gram = {A, B, C, D},
    ngrams(4, [], [Gram | Acc]);

ngrams(4, [A, B, C, D | Rest], Acc) ->
    Gram = {A, B, C, D},
    ngrams(4, [B, C, D | Rest], [Gram | Acc]);

%%%----------------------------------------------------------------------------
ngrams(5, [], Acc) ->
    lists:reverse(Acc);

ngrams(5, [A, B, C, D, E], Acc) ->
    Gram = {A, B, C, D, E},
    ngrams(5, [], [Gram | Acc]);

ngrams(5, [A, B, C, D, E | Rest], Acc) ->
    Gram = {A, B, C, D, E},
    ngrams(5, [B, C, D, E | Rest], [Gram | Acc]).

%%%----------------------------------------------------------------------------
analyze(2, [A, {_, U}], Acc) ->
    analyze(2, [], [{A, U} | Acc]);

analyze(2, [A, {_, U} = B | Rest], Acc) ->
    analyze(2, [B | Rest], [{A, U} | Acc]);

analyze(2, _, Acc) ->
    lists:reverse(Acc);

%%%----------------------------------------------------------------------------
analyze(3, [A, {_, _, U}], Acc) ->
    analyze(3, [], [{A, U} | Acc]);

analyze(3, [A, {_, _, U} = B | Rest], Acc) ->
    analyze(3, [B | Rest], [{A, U} | Acc]);

analyze(3, _, Acc) -> 
    lists:reverse(Acc);

%%%----------------------------------------------------------------------------
analyze(4, [A, {_, _, _, U}], Acc) ->
    analyze(4, [], [{A, U} | Acc]);

analyze(4, [A, {_, _, _, U} = B | Rest], Acc) ->
    analyze(4, [B | Rest], [{A, U} | Acc]);

analyze(4, _, Acc) -> 
    lists:reverse(Acc);

%%%----------------------------------------------------------------------------
analyze(5, [A, {_, _, _, _, U}], Acc) ->
    analyze(5, [], [{A, U} | Acc]);

analyze(5, [A, {_, _, _, _, U} = B | Rest], Acc) ->
    analyze(5, [B | Rest], [{A, U} | Acc]);

analyze(5, _, Acc) -> 
    lists:reverse(Acc).