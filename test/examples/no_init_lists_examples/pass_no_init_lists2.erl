-module(pass_no_init_lists2).

-behaviour(gen_server).

-export([start_link/0, init/1, init/2, handle_cast/2, handle_call/3]).

start_link() ->
  gen_server:start_link(?MODULE, #{a => map}, []).

init(#{}) ->
  ok.

init([_], undefined) ->
  ok.

handle_cast(_, _) -> ok.

handle_call(_, _, _) -> ok.
