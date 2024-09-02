-module(pass_unquoted_atoms).

-export([test/1, test/2]).

test(_Test) -> ok.

test(_A, nice_atom_name) -> perfect_atomname;
test(_A, _B) -> unquoted_atom.

