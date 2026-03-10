-module(fail_prefer_sigils).

-export([f/0]).

f() ->
    #{
        bit_string => <<1:2>>,
        bytes => <<$1, $\n, $2>>,
        binary_prefix => <<"1\n", $2>>,
        binary_suffix => <<$1, "\n2">>,
        binary => <<"1\n2">>,
        utf8 => <<"1\n2"/utf8>>,
        empty => <<>>,
        empty_2 => <<"">>,
        multiline => <<"""
1
2
""">>,
        string => "1\n2"
    }.
