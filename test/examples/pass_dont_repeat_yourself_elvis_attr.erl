-module(pass_dont_repeat_yourself_elvis_attr).

-export([
         repeated_complexity_5/2,
         repeated_complexity_10/2
        ]).

-elvis([{elvis_style, dont_repeat_yourself, #{min_complexity => 13}},
        {elvis_style, no_debug_call, disable},
        {elvis_style, function_naming_convention, #{regex => "^[a-z_0-9]+$"}}]).

repeated_complexity_5(X, Y) ->
    Z = X ++ [ok],
    W = Y ++ [ok],
    Z ++ W ++ [ok].

-spec repeated_complexity_10(any(), any()) -> fun((...) -> any()).
repeated_complexity_10(X, Y) ->
    Z = case X of
            Y -> io:format("Y");
            _ -> <<"ok">>
        end,

    W = case Z of
            X -> io:format("Y");
            _ -> <<"ok">>
        end,

    fun(_) -> W end.
