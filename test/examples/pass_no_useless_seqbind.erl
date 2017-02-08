-module(pass_no_useless_seqbind).

-compile({parse_transformer, seqbind}).

-dialyzer(no_match).

-export([demo/0]).

demo() ->
    X@ = 10,
    X@ = X@ + 1,
    X@.
