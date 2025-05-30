-module(elvis_ktn).

-export([value/1]).

value(Node) ->
    ktn_code:attr(value, Node).
