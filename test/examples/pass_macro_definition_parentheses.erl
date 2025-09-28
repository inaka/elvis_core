-module(pass_macro_definition_parentheses).

-export([get_attr/1]).

-define(CONSTANT, test).
-define(GET_NAME(), get_attr(name)).
-define(GET_AGE(), get_attr(age)).

get_attr(name) -> "John";
get_attr(age) -> 25.
