-module(pass_unquoted_atoms).

-export([test/1, test/2, test_unicode/0]).

test(_Test) -> ok.

test(_A, nice_atom_name) -> perfect_atomname;
test(_Reserved, _Words) -> ['after', 'and', 'andalso', 'band', 'begin', 'bnot', 'bor', 'bsl', 'bsr', 'bxor', 'case',
         'catch', 'cond', 'div', 'end', 'fun', 'if', 'let', 'not', 'of', 'or', 'orelse', 'receive',
         'rem', 'try', 'when', 'xor', 'maybe'].

%% Unicode atoms that genuinely need quotes should not crash the rule (GH-545)
test_unicode() -> 'Πλάτων'.
