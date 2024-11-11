-module(fail_no_successive_maps2).

-export([test1/0, test2/0]).

-if(?OTP_RELEASE<27).
test1() ->
  _ = #{
    x => [
        #{a => b, c => d},
        #{a => b, c => d} % expected a warning here
        #{a => b, c => d},
        #{a => b, c => d}
    ]
}.
-else.
test1() ->
    #{}.
-endif.

-if(?OTP_RELEASE<27).
test2() ->
  _ = #{
    map => #{
          x => [
              #{a => b, c => d},
              #{a => b, c => d} % expected a warning here
              #{a => b, c => d},
              #{a => b, c => d}
          ],
          y => [
              #{a => b, c => d},
              #{a => b, c => d} % expected a warning here
              #{a => b, c => d},
              #{a => b, c => d}
          ]
      }
}.
-else.
test2() ->
    #{}.
-endif.
