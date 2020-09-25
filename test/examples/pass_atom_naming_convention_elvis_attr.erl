-module(pass_atom_naming_convention_elvis_attr).

-export([for_test/0]).

-elvis([{elvis_style, atom_naming_convention, #{ regex => "^[a-zA-Z\_]+$",
                                                 enclosed_atoms => "^[a-zA-Z\_0-9' \-\\\\]+$" }},
        {elvis_style, line_length, #{limit => 100}}]).

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
    'backtick at the end\''.
