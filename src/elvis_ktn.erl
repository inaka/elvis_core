-module(elvis_ktn).

-export([pattern/1]).
-export([text/1]).
-export([tokens/1]).
-export([value/1]).

pattern(Node) ->
    ktn_code:node_attr(pattern, Node).

text(Node) ->
    ktn_code:attr(text, Node).

tokens(Node) ->
    ktn_code:attr(tokens, Node).

value(Node) ->
    ktn_code:attr(value, Node).
