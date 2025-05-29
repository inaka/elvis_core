-module(elvis_ktn).

-export([arity/1]).

arity(Fun) ->
  ktn_code:attr(arity, Fun).
