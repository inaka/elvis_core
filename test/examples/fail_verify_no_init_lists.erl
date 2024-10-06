-module(fail_verify_no_init_lists).

-export([start_link/1, init/1]).

start_link(AParam) ->
  gen_server:start_link(?MODULE, [AParam], []).

init([_AParam]) ->
  ok.
