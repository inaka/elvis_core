-module(pass_no_call_elvis_attr).
-export([fail/0]).

-elvis([{elvis_style, no_call, #{no_call_functions => [{io, format}]}}]).

-spec fail() -> any().
fail() ->
    _ = timer:send_after(0, fail_after),
    _ = timer:send_after(0, self(), fail_after),

    _ = timer:send_interval(0, fail_interval),
    _ = timer:send_interval(0, self(), fail_interval),

    _ = erlang:size({1, 2, 3}),
    _ = erlang:size(<<"fail_size">>),

    _ = erlang:tuple_size({1, 2, 3}),
    _ = erlang:byte_size(<<"ok_size">>).
