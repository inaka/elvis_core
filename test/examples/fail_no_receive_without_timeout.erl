-module(fail_no_receive_without_timeout).

-export([no_after/0, try_after/0, nested/0]).

no_after() ->
    receive X -> X end.

try_after() ->
    try
        receive X -> X end
    catch
        timeout -> timeout
    after
        this:is_not(the, 'after', "that you are looking for")
    end.

nested() ->
    receive
        good ->
            receive bad -> "This one doesn't have an after clause" end
    after 1_000 ->
        "This one is fine"
    end.
