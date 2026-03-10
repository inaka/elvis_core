-module(pass_expression_can_be_simplified).
-export([good/1]).
good(X) ->
    A = X + 1,
    B = [1] ++ [2, 3],
    C = X * 2,
    {A, B, C}.
