-module(pass_no_single_clause_case).

-export([simple/0, nested/0]).

simple() ->
    case do:something() of
        more -> than;
        one -> clause;
        is -> good
    end.

nested() ->
    case this:statement() of
        has -> multiple;
        clauses ->
            case this:nested(one) of
                has -> many;
                clauses ->
                    case as:well() andalso this:other() of
                        has -> two;
                        _Clauses -> too
                    end
            end
    end.
