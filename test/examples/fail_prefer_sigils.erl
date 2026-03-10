-module(fail_prefer_sigils).

-export([single/0, multi/0]).

single() ->
    #{
        bit_string => <<1:2>>,
        bytes => <<$1, $\n, $2>>,
        binary_prefix => <<"1\n", $2>>,
        binary_suffix => <<$1, "\n2">>,
        binary => <<"1\n2">>,
        utf8 => <<"1\n2"/utf8>>,
        empty => <<>>,
        empty_2 => <<"">>,
        string => "1\n2"
    }.

-if(?OTP_RELEASE >= 27).
multi() -> <<"""
1
2
""">>.
-else.
multi() -> "string_concats produce a warning in OTP26".
-endif.
