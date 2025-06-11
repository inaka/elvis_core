-module(fail_no_init_lists5).

-if(?OTP_RELEASE < 28).
-behaviour(gen_fsm).

-export([init/1, handle_sync_event/4, handle_event/3]).

init([]) -> {error, "Don't use list for init/1"}.

handle_sync_event(_, _, _, _) -> ok.

handle_event(_, _, _) -> ok.
-endif.
