-module(fail_no_match_in_condition).

-export([good/0, bad/0, ugly/0]).

good() ->
    case do:something() of
        #{a := A, b := good} ->
            {a, really, nice, A};
        #{a := A, b := bad} ->
            {"not", a, good, A}
    end.

bad() ->
    case #{a := A} = do:something() of
        #{b := good} ->
            {a, really, nice, A};
        #{b := bad} ->
            {"not", a, good, A}
    end.

ugly() ->
    case begin
             #{a := A} = do:something(),
             B = do:something('else', with, A)
         end
    of
        #{b := good} ->
            A;
        #{b := bad} ->
            B
    end.
