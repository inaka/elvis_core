-module(used_ignored_variable_in_macro).

-include_lib("stdlib/include/assert.hrl").

-define(MYMACRO(X), fun(X) -> ok end).

-export([do/0]).

do() ->
    List = [{a, b}, {c, d}],
    {_Key, b} = lists:keyfind(a, 1, List),
    ?assertMatch({_K, _}, lists:keyfind(a, 1, List)),
    ?MYMACRO({_P1, _}).
