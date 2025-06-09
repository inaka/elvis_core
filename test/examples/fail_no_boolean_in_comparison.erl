-module(fail_no_boolean_in_comparison).

-export [my_fun/2, other_examples/1].

my_fun(P1, P2) ->
    case do:something(P1) == false orelse do:something(P2) == false of
        true -> "There is a false";
        false -> "All true"
    end.

other_examples(A) ->
    %% An if clause
    X = if A == true -> "why do you do this?" end,

    %% A regular match operator
    Y = X == true,

    %% And a stricter one
    Y = X =:= true,

    %% And yet another (stricter) one
    Y1 = X =/= true,

    %% A list comprehension (different places)
    [ E || E <- build:a_list(), false == check:something_on(E)],

    %% An anonymous function head
    F = fun(X2, Y2) when X2 == true, false == Y2 -> {shake, my, head} end,
    F(X, Y),

    case the:result(F, Y1) of
        Bool when Bool == true; Bool /= false -> do:something_with(the, Bool);
        NotBool -> do:something_else_with(this, NotBool, thingy)
    end.
