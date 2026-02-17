-module(pass_code_complexity).

-export([simple_function/1, another_simple/1]).

%% Complexity: 1 (base) + 2 (case clauses - 1) = 3
simple_function(X) ->
    case X of
        a -> 1;
        b -> 2;
        _ -> 3
    end.

%% Complexity: 1
another_simple(X) when is_integer(X) ->
    X + 1.
