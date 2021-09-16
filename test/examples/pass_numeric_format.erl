-module(pass_numeric_format).

-export([for_test/0]).

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
