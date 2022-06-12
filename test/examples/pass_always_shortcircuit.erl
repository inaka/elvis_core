-module(pass_always_shortcircuit).

-export([in_guards/1, in_body/1]).

in_guards(X) when is_number(X) andalso X == 1 orelse X == 2 ->
    good_stuff.

in_body(X) ->
    is_list(X) andalso length(X) > 16 orelse length(X) == 0.
