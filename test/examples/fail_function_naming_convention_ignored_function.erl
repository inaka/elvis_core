-module(fail_function_naming_convention_ignored_function).

-dialyzer({nowarn_function, bad_names_inside/0}).

-export([bad_names_inside/0]).

%% Function names must use only lowercase characters.
%% Words in function names must be separated with _.

%% Cf. https://github.com/inaka/erlang_guidelines#function-names

bad_names_inside() ->
    camelCase(should, fail).

%% Private / hidden functions still checked

camelCase(Should, Fail) ->
    [Should, Fail].
