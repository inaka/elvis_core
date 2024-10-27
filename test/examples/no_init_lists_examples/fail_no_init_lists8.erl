-module(fail_no_init_lists8).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2]).

init([]) -> {error, "Don't use list for init/1"}.

handle_event(_, _) -> ok.

handle_call(_, _) -> ok.
