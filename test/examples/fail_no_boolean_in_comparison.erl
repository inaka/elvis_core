-module(fail_no_boolean_in_comparison).

-export [my_fun/2].

my_fun(P1, P2) ->
    case do:something(P1) == false orelse do:something(P2) == false of
        true -> "There is a false";
        false -> "All true"
    end.
