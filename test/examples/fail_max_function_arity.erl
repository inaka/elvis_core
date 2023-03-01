-module(fail_max_function_arity).

-export([f/0, f/1, f/2, f/3]).

f() ->
    f(1).

f(1) ->
    f(1, 2).

f(1, 2) ->
    f(1, 2, 3).

f(1, 2, 3) ->
    {1, 2, 3, 4}.
