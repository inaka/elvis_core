-module(pass_consistent_ok_error_spec).

-behaviour(supervisor).
-behaviour(pass_invalid_dynamic_call_callback).

-export([
         init/1,
         arity_two/0,
         arity_three/1,
         disabled_by_dynamic_behaviour/0,
         call/0,
         macro_function_name_call/0,
         list/0,
         ok/0,
         map/0,
         multi/1
        ]).

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Args) -> {ok, {#{strategy => one_for_one}, []}}.

-spec arity_two() -> number().
arity_two() -> rand:uniform().

-spec arity_three(X) -> {ok, X, X} | {error, X}.
arity_three(X) when X > 0 -> {ok, X, X};
arity_three(X) -> {error, X}.

-spec disabled_by_dynamic_behaviour() -> {ok, term()}.
disabled_by_dynamic_behaviour() -> {ok, disabled}.

-spec call() -> {ok, term()}.
call() -> {ok, callback}.

-spec macro_function_name_call() -> {ok, term()}.
macro_function_name_call() -> {ok, callback}.

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
