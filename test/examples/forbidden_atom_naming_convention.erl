-module(forbidden_atom_naming_convention).

-export([for_test/0]).

for_test() ->
    this_is_an_ok_atom,
    'and_so_is_this',
    'and_this_1_too',
    non_200,
    '_', % used by ets/mnesia/etc.
    non200, % valid, even without underscores
    valid_200even_if_numb3rs_appear_between_letters,
    blahblah_SUITE,
    x.
