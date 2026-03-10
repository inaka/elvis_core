-module(fail_expression_can_be_simplified).
-export([bad/1]).
bad(X) ->
    _ = [] ++ X,
    _ = X ++ [],
    _ = [] -- X,
    _ = X -- [],
    _ = X + 0,
    _ = X - 0,
    _ = 0 - X,
    _ = X * 1,
    _ = 1 * X,
    _ = X div 1,
    _ = X rem 1,
    _ = true andalso X,
    _ = false orelse X,
    _ = not true,
    _ = not false,
    _ = X band -1,
    _ = X bor 0,
    _ = X bxor 0,
    ok.
