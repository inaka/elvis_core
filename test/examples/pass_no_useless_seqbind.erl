-module(pass_no_useless_seqbind).

-compile({parse_transformer, seqbind}).

-export([demo/0]).

demo() ->
    X@ = 10,
    X@ = X@ + 1,
    X@.
