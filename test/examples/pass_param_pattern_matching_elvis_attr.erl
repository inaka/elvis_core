-module(pass_param_pattern_matching_elvis_attr).

-elvis([{elvis_style, param_pattern_matching, #{side => right}}]).

-export([my_fun/1, my_fun/3]).

my_fun(#{[variable, goes] := [on, the]} = Right) ->
    fun (#{[variable, goes] := [on, the]} = RightToo) ->
            Right == RightToo;
        ({can, be, Left} = {_If, _It, "is not a single variable"}) ->
            Left
    end;
my_fun({can, be, Left} = {_If, _It, "is not a single variable"}) ->
    Left.

my_fun(works, {as, well, on} = TheOther, parameters) ->
    fun(works, {as, well, on} = TheOtherToo, parameters) ->
        TheOther == TheOtherToo
    end.
