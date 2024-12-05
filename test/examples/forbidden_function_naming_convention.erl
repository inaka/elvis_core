-module(forbidden_function_naming_convention).

-dialyzer({nowarn_function, bad_names_inside/0}).

-export([bad_names_inside/0]).

%% Function names must use only lowercase characters.
%% Words in function names must be separated with _.

%% Cf. https://github.com/inaka/erlang_guidelines#function-names

bad_names_inside() ->
    good_name(should, fail),
    not_forbidden_but_still_bad____(should, fail),
    no_numbers_1(should, fail),
    fun2ms(should, fail).

%% Private / hidden functions still checked

good_name(Should, Fail) ->
    [Should, Fail].

not_forbidden_but_still_bad____(Should, Fail) ->
    [Should, Fail].

no_numbers_1(Should, Fail) ->
    [Should, Fail].

fun2ms(Should, Fail) ->
    [Should, Fail].

