%%% -*- coding: latin-1 -*-
-module(fail_line_length_latin1).

-export([
         function_1/0,
         function_2/0
        ]).

function_1() ->
    io:format("Hellö").

function_2() ->
    io:format("Hellö, This line is 81 characters long and should be detected, yeah!!!").

