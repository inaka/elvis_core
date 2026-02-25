-module(fail_no_debug_call_mixed_arity).
-export([fail/0, fail/1, fail/2]).

%% Used to test partial ignore: ignore only arity 0 and verify arity 2 still reports.

-spec fail() -> any().
fail() ->
    io:format("debug in fail/0~n").

-spec fail(atom()) -> any().
fail(_A) ->
    io:format("debug in fail/1~n"),
    erlang:display("also debug").

-spec fail(atom(), integer()) -> any().
fail(_A, _N) ->
    io:format("debug in fail/2~n"),
    io:format("more debug in fail/2~n"),
    erlang:display("also debug").
