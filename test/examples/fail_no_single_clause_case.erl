-module(fail_no_single_clause_case).

-export([simple/0, nested/0]).

simple() ->
    case do:something() of
        {just, one, clause} -> {is, bad}
    end.

nested() ->
    case this:statement() of
        has -> multiple;
        clauses ->
            case but:this(one, inside) of
                has ->
                    case just:one() of
                        {this, one} ->
                            case also:has(one) of
                                but -> the;
                                one -> "in the middle of everything";
                                has -> many
                            end
                    end
            end
    end.
