-module(custom_ms_transform_included).

-export([test/0]).

test() -> {this, is, "not", ets, my:fun2ms("It's my own")}.
