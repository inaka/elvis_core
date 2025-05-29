-module(elvis_ktn).

-export([arity/1]).
-export([line/1]).
-export([location/1]).
-export([name/1]).

arity(Node) ->
    ktn_code:attr(arity, Node).

line(Node) ->
    {Line, _Col} = location(Node),
    Line.

location(Node) ->
    ktn_code:attr(location, Node).

name(Node) ->
    ktn_code:attr(name, Node).
