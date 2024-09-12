-module(fail_atom_naming_convention).

-export([for_test/0]).

for_test() ->
    this_is_not_an_OK_atom,
    'and_neither-is_this',
    'or_THIS',
    '1_of_us_is_wrong',
    '\' this nasty atom\'',
    '\'',
    '\'\'',
    '\'startswithbacktick',
    'backtick\'inside',
    'backtick at the end\'',
    non200_,
    '__', % invalid, even when '_' is actually valid
    two_underscores__together_are_not_valid,
    '_something', % invalid because it starts with underscore
    '42_invalid_because_it_starts_with_a_number',
    '42invalid', %% even without underscores
    weDontSupportCamelCaseHere.
