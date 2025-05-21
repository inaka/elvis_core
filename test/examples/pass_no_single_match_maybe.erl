-module(pass_no_single_match_maybe).

-feature(maybe_expr, enable).

-export([simple/0, with_else/0]).

simple() ->
    maybe
        {this, statement} ?= has:multiple(),
        {clauses, thus} ?= it:is(),
        ok
    end.

with_else() ->
    maybe
        {this, statement} ?= has:a_single(clause, but, it, has, an)
    else
        clause -> too
    end.
