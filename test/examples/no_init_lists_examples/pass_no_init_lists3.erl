-module(pass_no_init_lists3).

-behaviour(gen_server).

-export([start_link/0, init/0, init/1, handle_cast/2, handle_call/3]).

start_link() ->
  gen_server:start_link(?MODULE, {1, 2}, []).

init({1, 2}) ->
  ok.

init() ->
  ok.

handle_cast(_, _) -> ok.

handle_call(_, _, _) -> ok.
