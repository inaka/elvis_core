-module(pass_line_length_elvis_attr).

-elvis([{elvis_text_style, line_length, #{limit => 189}}]).
-elvis([{elvis_style, no_debug_call, disable}]).
-elvis([{elvis_style, function_naming_convention, #{regex => "^function_[0-9]+$"}}]).

-export([
         function_1/0,
         function_2/0,
         function_3/0,
         function_4/0,
         function_5/0,
         function_6/2,
         function_7/0
        ]).

% Single line comment over 100 characters!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function_1() ->
    io:format("Hello").

function_2() ->
    io:format("This line is 101 characters long and should be detected, yeah!!!!!!!!!!!!!!!!!!!!!!").

function_3() ->
    io:format("This line is 100 characters long and shouldn't be detected!!!!!!!!!!!!!!!!!!!!!!!!").

function_4() ->
    %% Single line comment with spaces in front that is over 100 characters. So many test cases!!!!!!!!!!!!!!!!!!!!!!!
    io:format("This line is 90 characters long and should be detected!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!").

function_5() ->
    io:format("This line is 90 characters long and should be detected ~p!!!!!!!!!!!!!!!!!!!!!!!!!!", [yeah]),
    io:format("This line is 90 characters long and should be detected ~p and these two escaped ~p!!!!!!!!!!!!!!!!!!", [yeah, no]).

function_6(Config, AntPositions) ->
    gb_trees:from_orddict([{Pos, #{pos => Pos, state => model:random_ant_state(Config)}} || Pos <- lists:sort(AntPositions)]). % {Pozycja, CałyAgent} - ew. do zmiany, jest zbalansowany [DG]

function_7() ->
    io:format("Going to put a comment after this..."), % This comment pushes the line past 100 characters...
    ok.
