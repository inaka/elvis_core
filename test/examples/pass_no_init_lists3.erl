-module(pass_no_init_lists3).

-behaviour(gen_statem).

-export([start_link/1, init/1, handle_cast/2, handle_call/3, callback_mode/0]).

start_link(B) ->
  gen_server:start_link(?MODULE, [B], []).

init([_B]) ->
  ok.

handle_cast(_, _) -> ok.

handle_call(_, _, _) -> ok.

callback_mode() -> ok.
