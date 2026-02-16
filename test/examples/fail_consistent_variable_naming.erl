-module(fail_consistent_variable_naming).

-export_type([one/1, two/1]).
-export([one/1, two/1]).
-export([ignored/0]).
-export([my_fun/1]).

%% Inconsistency in types
-type one(TypeVar) :: {one, TypeVar}.
-type two(Type_var) :: {two, Type_var}.

%% In specs and function arguments
-spec one(SpecVar) -> {one, SpecVar}.
one(FuncVar) -> {one, FuncVar}.

-spec two(SPEc_Var) -> {two, SPEc_Var}.
two(FUnc_Var) -> {two, FUnc_Var}.

ignored() ->
    One = fun(IgnVar) -> {one, IgnVar} end,
    Two = fun(_IGN_Var) -> two end,
    General = fun(_GeNeRaL_iNcOnSiStEnCy) -> general end,
    General(Two(One(inconsistency))).

my_fun(MyVar) ->
    my_fun(MyVar, too).

my_fun(My_Var, too) ->
    {My_Var, too}.
