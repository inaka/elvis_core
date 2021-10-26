-module(fail_atom_naming_convention_exception_class).

-export([for_test/0]).

for_test() ->
    try wok catch Reason -> Reason end.
