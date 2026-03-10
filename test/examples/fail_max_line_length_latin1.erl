%%% -*- coding: latin-1 -*-
-module(fail_max_line_length_latin1).

-export([
         function_1/0,
         function_2/0
        ]).

function_1() ->
    io:format("Hell�").

function_2() ->
    io:format("Hell�, This line is 101 characters long and should be detected, yeah!!!!!!!!!!!!!!!").
