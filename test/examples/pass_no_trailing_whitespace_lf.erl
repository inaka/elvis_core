-module(pass_no_trailing_whitespace_lf).

-export([one/0]).

one() ->
    %% Following line should be fine
    ok.
