-module(fail_max_non_exported_function_arity).

-export([f/0, f/1]).

f() ->
    f(1).

f(1) ->
    f(1, 2).

f(1, 2) ->
    f(1, 2, 3).

f(1, 2, 3) ->
    f(1, 2, 3, 4).

f(1, 2, 3, 4) ->
    {1, 2, 3, 4, 5}.
