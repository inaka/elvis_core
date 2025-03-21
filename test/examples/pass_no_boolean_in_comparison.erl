-module(pass_no_boolean_in_comparison).

-export [my_fun/2, my_fun/3, my_fun/4, my_fun/5].

my_fun(P1, P2) ->
    case (not do:something(P1)) orelse (not do:something(P2)) of
        true -> "There is a false";
        false -> "All true"
    end.

%% … or even …

my_fun(P1, P2, _) ->
    case do:something(P1) andalso do:something(P2) of
        true -> "All true";
        false -> "There is a false"
    end.

%% … or even …

my_fun(P1, P2, _, _) ->
    case do:something(P1) == internal_fun(true) orelse do:something(P2) == internal_fun(false) of
        true -> "There is a false";
        false -> "All true"
    end.

internal_fun(Boolean) -> Boolean.

%% … or even …

my_fun(P1, P2, _, _, _) ->
    case do:something(P1) == get_boolean(0) orelse do:something(P2) == get_boolean(1) of
        true -> "There is a false";
        false -> "All true"
    end.

get_boolean(0) -> true;
get_boolean(1) -> false.
