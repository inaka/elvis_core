-module(elvis_ktn).

-export([tokens/1]).
-export([value/1]).

tokens(Node) ->
    ktn_code:attr(tokens, Node).

value(Node) ->
    ktn_code:attr(value, Node).
