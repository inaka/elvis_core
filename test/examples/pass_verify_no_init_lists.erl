-module(pass_verify_no_init_lists).

-export([start_link/0, init/1]).

start_link() ->
  gen_server:start_link(?MODULE, undefined, []).

init(_) ->
  ok.
