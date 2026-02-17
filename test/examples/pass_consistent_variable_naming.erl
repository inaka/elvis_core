-module(pass_consistent_variable_naming).

-export_type([one/1, two/1, general/1]).
-export([one/1, two/1]).
-export([my_fun/1, my_fun/2]).
-export([ignored/0]).

%% Inconsistency in types
-type one(TypeVar) :: {one, TypeVar, 'Typevar'}.
-type two(TypeVar) :: {two, TypeVar, 'TypeVAR'}.
-type general(GeneralConsistency) :: {general, GeneralConsistency, 'GeneralCONSISTENCY'}.

%% In specs and function arguments
-spec one(SpecVar) -> {one, SpecVar}.
one(FuncVar) -> {one, FuncVar}.

-spec two(SpecVar) -> {two, SpecVar}.
two(FuncVar) -> {two, FuncVar}.

my_fun(MyVar) ->
    my_fun(MyVar, too).

my_fun(MyVar, MyOtherVar) ->
    {MyVar, MyOtherVar}.

ignored() ->
    One = fun(IgnVar) -> {one, IgnVar} end,
    Two = fun(_IgnVar) -> two end,
    General = fun(_GeneralConsistency) -> general end,
    General(Two(One(inconsistency))).
