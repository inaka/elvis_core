-module(fail_no_single_match_maybe).

-feature(maybe_expr, enable).

-export([simple/0, nested/0]).

simple() ->
    maybe
        {just, one, clause, is, bad} ?= do:something()
    end.

nested() ->
    maybe
        {this, statement} ?= has:multiple(),
        {clauses, thus} ?= its:ok(),
        maybe
            {this, one} ?= maybe
                {and_this, one} ?= also:has(just, one)
            end
        end
    end.
