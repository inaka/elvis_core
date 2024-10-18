-module(pass_no_init_lists5).

-behaviour(example_behaviour).

-export([init/1, example/0]).

init([]) -> {error, "can be a list"}.
example() -> ok.
