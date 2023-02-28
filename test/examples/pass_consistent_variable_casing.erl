-module(pass_consistent_variable_casing).

-export_type([one/1, two/1, general/1]).
-export([one/1, two/1, general/1]).
-export([funs/0]).
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

-spec general(GeneralConsistency) -> {general, GeneralConsistency}.
general(GeneralConsistency) -> {general, GeneralConsistency}.

funs() ->
    One = fun(FunVar) -> {one, FunVar} end,
    Two = fun(FunVar) -> {two, FunVar} end,
    General = fun(GeneralConsistency) -> {general, GeneralConsistency} end,
    General(Two(One(inconsistency))).

ignored() ->
    One = fun(IgnVar) -> {one, IgnVar} end,
    Two = fun(_IgnVar) -> two end,
    General = fun(_GeneralConsistency) -> general end,
    General(Two(One(inconsistency))).
