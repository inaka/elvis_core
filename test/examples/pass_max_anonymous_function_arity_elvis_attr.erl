-module(pass_max_anonymous_function_arity_elvis_attr).

-export([f/0]).

-elvis([{elvis_style, max_anonymous_function_arity, #{ max_arity => 10 }}]).

f() ->
    fun(X1, X2, X3, X4, X5, X6, X7, X8, X9, X0) ->
        {X1, X2, X3, X4, X5, X6, X7, X8, X9, X0}
    end.
