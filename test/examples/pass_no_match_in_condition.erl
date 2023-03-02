-module(pass_no_match_in_condition).

-export([try_body/0, good_case/0]).

try_body() ->
    try
        It = is:fine(to, have),
        a:try_statement(with, a, match, in, It)
    catch
        _ ->
            not_bad
    end.

good_case() ->
    case it:is(fine) of
        To = {have, matches} ->
            in:clause(heads, To);
        o ->
            ":P"
    end.
