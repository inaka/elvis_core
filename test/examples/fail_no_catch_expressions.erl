-module(fail_no_catch_expressions).

-export([catchf/0, try_catch/0, mixem/0, discarded/0]).

-dialyzer({nowarn_function, [catchf/0, try_catch/0, mixem/0, discarded/0]}).

catchf() ->
    F = fun (a) -> "catch" end,
    catch F(b).

try_catch() ->
    try
        F = fun (_) -> lists:sort(fff) end,
        F(b)
    catch
        _ ->
            'catch', % and there's catch inside a comment
            catchf()
    end.

mixem() ->
    F = fun (a) -> ok end,
    try
        catch F(b)
    catch _ ->
        catch F(a)
    end.

discarded() ->
    _ = catch this:should(be, fine).
