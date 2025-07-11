-module(fail_simplify_anonymous_functions).

-export([functions/0]).

functions() ->
    #{
        no_args => fun() -> rand:uniform() end,
        one_arg => fun(X) -> erlang:display(X) end,
        two_args => fun(A, B) -> io:format(A, B) end,
        local => fun() -> local() end,
        auto_import => fun(A) -> atom_to_list(A) end
    }.

local() -> local.
