-module(fail_no_hrl_include).
-include("no_hrl_include.hrl").

-export([hello/1]).

-spec hello(test_type()) -> string().
hello(_A) ->
    "Hello !".