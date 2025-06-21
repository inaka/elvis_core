-module(fail_operator_spaces_otp28).

-if(?OTP_RELEASE >= 28).
-export([fail_generators/0]).

fail_generators() ->
    [Integer || {Integer, _} <:-[{1, 2}, {3, 4}]],
    [Word || <<Word:16>><:= <<16#1234:16, 16#ABCD:16>>],
    #{K => V + 1 || K := V<:-#{a => 1, b => 2}},
    [A + B || A <- [1, 2, 3]&&B <- [4, 5, 6]].
-endif.
