-module(pass_no_trailing_whitespace_crlf).

-export([one/0]).

one() ->
    %% Following line should be fine but file ends in \r\n
    ok.
