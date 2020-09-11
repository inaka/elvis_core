-module(pass_atom_naming_convention).

-export([for_test/0]).

for_test() ->
    this_is_an_ok_atom,
    'and_so_is_this',
    'and_this_1_too'.
