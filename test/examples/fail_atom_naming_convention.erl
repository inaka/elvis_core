-module(fail_atom_naming_convention).

-export([for_test/0]).

for_test() ->
    'this_is_not_an_OK_atom',
    'and_neither-is_this',
    'or_THIS',
    '1_of_us_is_wrong'.
