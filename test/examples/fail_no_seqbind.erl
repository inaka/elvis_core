-module(fail_no_seqbind).

-compile({parse_transform, seqbind}).
-compile([{parse_transform, seqbind}]).
-compile([export_all, {parse_transform, seqbind}]).
