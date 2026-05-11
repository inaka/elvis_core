-module(fail_consistent_ok_error_spec).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, arity_two/0, arity_three/1, multi/1]).

-type state() :: #{}.

-spec init(noargs) -> {ok, state()}.
init(noargs) -> {ok, #{}}.

-spec handle_call(term(), term(), state()) -> {noreply, state()}.
handle_call(_Request, _From, State) -> {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) -> {noreply, State}.

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
