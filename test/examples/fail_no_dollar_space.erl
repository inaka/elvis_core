-module(fail_no_dollar_space).

-export([good/1, bad/1]).

good(Char) when Char == $\s ->
    "Using" ++ [$\s | "$\\s is good"].

bad(Char) when Char == $ ->
    "Using " ++ [$  | "$  is confusing"].
