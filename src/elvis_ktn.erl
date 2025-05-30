-module(elvis_ktn).

-export([line/1]).
-export([location/1]).
-export([module/1]).
-export([name/1]).
-export([operation/1]).
-export([pattern/1]).
-export([text/1]).
-export([tokens/1]).
-export([value/1]).

line(Node) ->
    {Line, _Col} = location(Node),
    Line.

location(Node) ->
    ktn_code:attr(location, Node).

module(Node) ->
    ktn_code:node_attr(module, Node).

name(Node) ->
    ktn_code:attr(name, Node).

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
