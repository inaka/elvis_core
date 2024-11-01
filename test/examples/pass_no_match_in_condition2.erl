-module(pass_no_match_in_condition2).

-export([valid/1]).

valid(List) ->
    case lists:all(fun ({K, V}) ->
                           Something = find:something(for, K),
                           check:something(V, Something)
                   end,
                   List)
        of
        true -> all_true;
        false -> found_a_bad_one
    end.
