-module(fail_quoted_atoms).

-export([test/1, test/2]).

-define(TEST(), test(1, default)).

test(_Test) -> {ok, test}.

test(_A, 'ugly_atom_name') -> 'why_use_quotes_here';
test(_A, default) -> ?TEST();
test(_A, _B) -> 'and'.
