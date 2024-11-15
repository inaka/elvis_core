-module(pass_ms_transform).

-export([test/0]).

test() -> ets:fun2ms(fun() -> ok end).
