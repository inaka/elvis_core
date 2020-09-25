-module(pass_no_behavior_info_elvis_attr).

-elvis([{elvis_style, no_behavior_info, disable}]).
-elvis([{elvis_style, dont_repeat_yourself, disable}]).

-export([
         behavior_info/1,
         behaviour_info/1
        ]).

-export([
         foo/0,
         bar/1,
         baz/2
        ]).

behavior_info(callbacks) ->
    [{foo, 0}, {bar, 1}, {bax, 2}].

behaviour_info(callbacks) ->
    [{foo, 0}, {bar, 1}, {bax, 2}].

foo() ->
    ok.

bar(_Arg) ->
    ok.

baz(_Arg, _ArgTwo) ->
    ok.
