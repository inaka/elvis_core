-module(pass_no_nested_try_catch).

-export([example/0]).

%% @doc If this fails in do:something/0, we handle the error.
%%      But if it fails in do:something_else/2, we ignore it.
example() ->
    try do:something() of
        {ok, KeepGoing} ->
            try
                do:something_else("and", KeepGoing)
            catch
                _:_ -> {ignore, errors, here}
            end
    catch
        Kind:Error ->
            try this:block(is, also, not_nested) catch _:_ -> {Kind, Error} end
    after
        try this:one(is, "not", nested, either) catch _:_ -> ok end
    end.
