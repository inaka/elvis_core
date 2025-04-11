-module(pass_function_naming_convention).

-export([snake_case/2, has_digit1/2]).

%% Function names must use only lowercase characters.
%% Words in function names must be separated with _.

%% Cf. https://github.com/inaka/erlang_guidelines#function-names

has_digit1(Should, Pass) ->
    [Should, Pass].

snake_case(Should, Pass) ->
    [Should, Pass].

-include_lib("eunit/include/eunit.hrl").

%% A function with a name ending in ..._test_() (note the final underscore) is 
%% recognized by EUnit as a test generator function. 
%% Test generators return a representation of a set of tests to be executed by EUnit.
basic_test_() ->
    fun () -> ?assert(1 + 1 =:= 2) end.
