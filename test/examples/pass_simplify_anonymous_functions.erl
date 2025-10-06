-module(pass_simplify_anonymous_functions).

-export([functions/0]).

functions() ->
    #{
        no_args => fun rand:uniform/0,
        one_arg => fun erlang:display/1,
        two_args => fun io:format/2,
        local => fun local/0,
        auto_import => fun atom_to_list/1,
        flip_param_order => fun(A, B) -> io:format(B, A) end,
        more_than_a_call => fun() -> second:call(first:call()) end,
        op => fun(X) -> not X end,
        bi_op => fun(A, B) -> A + B end,
        id => fun(X) -> X end,
        with_guards => fun(X) when is_binary(X) -> binary_to_list(X) end,
        with_matching => fun(<<X/binary>>) -> erlang:binary_to_atom(X) end,
        multiple_clauses => fun (x) -> atom_to_list(x); (X) -> binary_to_list(X) end,
        ignored_arg => fun(X, _) -> erlang:display(X) end,
        in_a_case => case spawn(fun erlang:halt/0) of Pid -> Pid end
    }.

local() -> local.
