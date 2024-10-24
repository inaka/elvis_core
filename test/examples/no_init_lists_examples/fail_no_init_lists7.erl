-module(fail_no_init_lists7).

-behaviour(supervisor_bridge).

-export([init/1, terminate/2]).

init([]) -> {error, "Don't use list for init/1"}.

terminate(_, _) -> ok.
