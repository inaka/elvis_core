-module(fail_consistent_ok_error_spec).

-export([arity_two/0, arity_three/1, multi/1]).

-spec arity_two() -> {ok, number()}.
arity_two() -> {ok, rand:uniform()}.

-spec arity_three(X::number()) -> {ok, X, X}.
arity_three(X) when X > 0 -> {ok, X, X};
arity_three(X) -> {error, X}.

-spec multi
    (0) -> {ok, 0};
    (1) -> {ok, 1}.
multi(0) -> {ok, 0};
multi(1) -> {ok, 1}.
