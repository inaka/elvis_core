-module(pass_consistent_ok_error_spec).

-export([arity_two/0, arity_three/1, list/0, ok/0, map/0, multi/1]).

-spec arity_two() -> number().
arity_two() -> rand:uniform().

-spec arity_three(X) -> {ok, X, X} | {error, X}.
arity_three(X) when X > 0 -> {ok, X, X};
arity_three(X) -> {error, X}.

-spec list() -> [ok | number()].
list() -> [ok].

-spec ok() -> ok.
ok() -> ok.

-spec map() -> #{ok := number()}.
map() -> #{ok => rand:uniform()}.

-spec multi
    (0) -> {ok, 0};
    (1) -> {error, 1}.
multi(0) -> {ok, 0};
multi(1) -> {error, 1}.
