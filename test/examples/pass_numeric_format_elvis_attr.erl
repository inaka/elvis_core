-module(pass_numeric_format_elvis_attr).

-export([for_test/0]).

-elvis([{elvis_style, numeric_format, #{regex => "^[^_]+$"}}]).

for_test() ->
    { 1
    , 2
    , 10
    , 100
    , 1000
    , 2#1010101
    , 16#FACE
    , 1.1010101
    , 0.34e12
    , -1
    , -10
    , -1000
    , -2#1010101
    , -16#FACE
    , -1.1010101
    , -0.34e-123
    }.
