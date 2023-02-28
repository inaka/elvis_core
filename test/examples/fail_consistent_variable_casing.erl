-module(fail_consistent_variable_casing).

-export_type([one/1, two/1, general/1]).
-export([one/1, two/1, general/1]).
-export([funs/0]).
-export([ignored/0]).

%% Inconsistency in types
-type one(TypeVar) :: {one, TypeVar}.
-type two(Typevar) :: {two, Typevar}.
-type general(GeneralInconsistency) :: {general, GeneralInconsistency}.

%% In specs and function arguments
-spec one(SpecVar) -> {one, SpecVar}.
one(FuncVar) -> {one, FuncVar}.

-spec two(SPECVar) -> {two, SPECVar}.
two(FUNCVar) -> {two, FUNCVar}.

-spec general(GENERALInconsistency) -> {general, GENERALInconsistency}.
general(GENERALInconsistencY) -> {general, GENERALInconsistencY}.

funs() ->
    One = fun(FunVar) -> {one, FunVar} end,
    Two = fun(FunVAR) -> {two, FunVAR} end,
    General = fun(GeneralINCONSISTENCY) -> {general, GeneralINCONSISTENCY} end,
    General(Two(One(inconsistency))).

ignored() ->
    One = fun(IgnVar) -> {one, IgnVar} end,
    Two = fun(_IGNVar) -> two end,
    General = fun(_GeNeRaLiNcOnSiStEnCy) -> general end,
    General(Two(One(inconsistency))).
