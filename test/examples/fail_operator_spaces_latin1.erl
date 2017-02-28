%%% -*- coding: latin-1 -*-
-module(fail_operator_spaces_latin1).

-export([
         function_1/0
        ]).

function_1() ->
    "ö"++"1".
