-module(fail_ms_transform_included).

-export([test/0]).

test() -> ets:fun2ms(fun(_) -> ok end).
