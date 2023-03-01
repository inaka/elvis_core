-module(pass_max_function_arity).

-export([f/0, f/1, f/2, f/3, f/4, f/5, f/6, f/7, f/8]).

f() ->
    f(1).

f(1) ->
    f(1, 2).

f(1, 2) ->
    f(1, 2, 3).

f(1, 2, 3) ->
    f(1, 2, 3, 4).

f(1, 2, 3, 4) ->
    f(1, 2, 3, 4, 5).

f(1, 2, 3, 4, 5) ->
    f(1, 2, 3, 4, 5).

f(1, 2, 3, 4, 5, Six) ->
    f(1, 2, 3, 4, 5, Six, seven).

f(1, 2, 3, 4, 5, Six, seven) ->
    f(1, 2, 3, 4, 5, Six, seven, "eight").

f(1, 2, 3, 4, 5, Six, seven, "eight") ->
    {1, 2, 3, 4, 5, Six, seven, "eight"}.
