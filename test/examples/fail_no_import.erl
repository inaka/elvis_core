-module(fail_no_import).

-import(io, [format/1, format/2]).
-import(lists, [sum/1, seq/2]).

-export([f/0]).

f() ->
    format("This should use io:format/1"),
    format("This should also use io: and lists: ~p", [sum(seq(1, 10))]).
