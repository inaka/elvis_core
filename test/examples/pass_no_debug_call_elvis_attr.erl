-module(pass_no_debug_call_elvis_attr).
-export([fail/0]).

-ignore_xref({cthr, pal, 1}).
-ignore_xref({cthr, pal, 2}).

-elvis([{elvis_style, no_debug_call, #{debug_functions => [{ct, log}]}}]).

-spec fail() -> any().
fail() ->
    io:format("debug print~n"),
    io:format("debug print ~s~n", ["debug info"]),
    % Sending explicit io to a device is not considered debugging
    io:format(user, "Not a debug print ~s~n", ["Not debug"]),
    ct:pal("Debug"),
    ct:pal("Debug ~s", ["Debug Info"]),
    ct:print("Debug"),
    ct:print("Debug ~s", ["Debug Info"]).
