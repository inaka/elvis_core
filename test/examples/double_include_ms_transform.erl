-module(double_include_ms_transform).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([test/0]).

test() -> ets:fun2ms(fun(_) -> ok end).
