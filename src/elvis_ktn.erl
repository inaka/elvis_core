-module(elvis_ktn).

-export([operation/1]).
-export([pattern/1]).
-export([text/1]).
-export([tokens/1]).
-export([value/1]).

operation(Node) ->
    ktn_code:attr(operation, Node).

pattern(Node) ->
    ktn_code:node_attr(pattern, Node).

text(Node) ->
    ktn_code:attr(text, Node).

tokens(Node) ->
    ktn_code:attr(tokens, Node).

value(Node) ->
    ktn_code:attr(value, Node).
