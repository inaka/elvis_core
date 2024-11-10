-module(fail_no_successive_maps2).

-export([test1/0, test2/0]).

test1() ->
  _ = #{
    x => [
        #{a => b, c => d},
        #{a => b, c => d} % expected a warning here
        #{a => b, c => d},
        #{a => b, c => d}
    ]
}.

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
