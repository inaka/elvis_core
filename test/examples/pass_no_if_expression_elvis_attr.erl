-module(pass_no_if_expression_elvis_attr).

-dialyzer(no_match).

-elvis([{elvis_style, no_if_expression, disable}]).
-elvis([{elvis_style, dont_repeat_yourself, disable}]).

-export([
         uses_if/1,
         uses_if_twice/1
        ]).

uses_if(Arg) ->
    if
        Arg -> ok;
        not Arg -> not_ok
    end,
    case 1 of
        1 -> ok;
        2 -> ok;
        3 -> ok
    end.

uses_if_twice(Arg) ->
    if
        Arg -> ok;
        not Arg -> not_ok
    end,
    case 1 of
        1 -> ok;
        2 -> ok;
        3 -> ok
    end,
    if
        Arg -> ok;
        not Arg -> not_ok
    end.
