-module(pass_no_match_in_condition3).

-export([valid/1]).

valid(List) ->
    case
        do_some(
            begin
                Something = find:something(for, foo),
                check:something(bar, Something)
            end,
            List
        )
    of
        true -> all_true;
        false -> found_a_bad_one
    end.

do_some(_Do, _List) ->
    true.
