-module(fail_no_hrl_include).
-include("no_hrl_include.hrl").

-export([hello/0]).

hello() ->
    "Hello !".