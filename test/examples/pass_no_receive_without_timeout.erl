-module(pass_no_receive_without_timeout).

-define(TIMEOUT, 1_000).

-export([literal/0, macro/0, variable/0, function_call/0, nested/0]).

literal() ->
    receive X -> X
    after 1_000 -> ok
    end.

macro() ->
    receive X -> X
    after ?TIMEOUT -> ok
    end.

variable() ->
    Timeout = 1_000,
    receive X -> X
    after Timeout -> ok
    end.

function_call() ->
    receive X -> X
    after default:timeout() -> ok
    end.

nested() ->
    try
        receive X -> X
        after 10_000 -> throw(timeout)
        end
    catch
        timeout -> timeout
    end.
