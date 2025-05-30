-module(elvis_ktn).

-export([text/1]).
-export([tokens/1]).
-export([value/1]).

text(Node) ->
    ktn_code:attr(text, Node).

tokens(Node) ->
    ktn_code:attr(tokens, Node).

value(Node) ->
    ktn_code:attr(value, Node).
