-module(fail_macro_definition_parentheses).

-export([get_attr/1]).

-define(CONSTANT(), 5).
-define(GET_NAME, get_attr(name)).
-define(GET_AGE, get_attr(age)).

get_attr(name) -> "John";
get_attr(age) -> 25.
