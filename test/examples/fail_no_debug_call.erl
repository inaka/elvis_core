-module(fail_no_debug_call).
-export([fail/0]).

-define(DBG(Fmt, Args), io:format(Fmt, Args)).

-spec fail() -> any().
fail() ->
    erlang:display("debug print"),
    io:format("debug print~n"),
    ?DBG("debug print ~s~n", ["debug info"]),
    % Sending explicit io to a device is not considered debugging
    io:format(user, "Not a debug print ~s~n", ["Not debug"]),
    ct:pal("Debug"),
    ct:pal("Debug ~s", ["Debug Info"]),
    ct:print("Debug"),
    ct:print("Debug ~s", ["Debug Info"]),
    io:put_chars("Debug").
