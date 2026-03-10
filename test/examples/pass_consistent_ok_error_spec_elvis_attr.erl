-module(pass_consistent_ok_error_spec_elvis_attr).

-elvis([{elvis_style, consistent_ok_error_spec, disable}]).

-export([arity_two/0, arity_three/1]).

-spec arity_two() -> {ok, number()}.
arity_two() -> {ok, rand:uniform()}.

-spec arity_three(X) -> {ok, X, X}.
arity_three(X) when X > 0 -> {ok, X, X};
arity_three(X) -> {error, X}.
