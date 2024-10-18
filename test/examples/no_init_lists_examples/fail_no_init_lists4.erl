-module(fail_no_init_lists4).

-behaviour(gen_server).
-behaviour(example_behaviour).

-export([init/1, handle_cast/2, handle_cast/3, handle_call/3]).
-export([example/0]).

init([]) -> {error, "should not be a list"}.

handle_cast(_, _) -> ok.
handle_cast(_, _, _) -> ok.
handle_call(_, _, _) -> ok.

example() -> ok.
