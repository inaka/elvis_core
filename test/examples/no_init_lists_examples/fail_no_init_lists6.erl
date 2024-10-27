-module(fail_no_init_lists6).

-behaviour(supervisor).

-export([init/1]).

init([]) -> {error, "Don't use list for init/1"}.
