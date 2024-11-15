-module(pass_ms_transform_included).

-include_lib("stdlib/include/ms_transform.hrl").

-export([test/0]).

test() -> ets:fun2ms(fun(_) -> ok end).
