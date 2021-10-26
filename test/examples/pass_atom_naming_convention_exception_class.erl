-module(pass_atom_naming_convention_exception_class).

-export([for_test/0]).

for_test() ->
    try ok catch {pass} = This -> This end,
    try ok catch Throw -> {error, Throw}end,
    try ok catch R1 -> R1 end,        % no atoms with w, x, y, h, nor r
    try 'throw' catch R2 -> R2 end,   % the atom with w is enclosed
    try ok catch throw:R3 -> R3 end,  % throw is not an atom
    try ok catch error:R4 -> R4 end,  % error is not an atom
    try ok catch exit:R5 -> R5 end,   % exit is not an atom
    try throw(ok) catch R6 -> R6 end. % throw is not an atom either
