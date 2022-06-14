-module(fail_always_shortcircuit).

-export([in_guards/1, in_body/1]).

in_guards(X) when is_number(X) and (X == 1) or (X == 2) ->
    bad_stuff.

in_body(X) ->
    is_list(X) and (length(X) > 16) or (length(X) == 0).
