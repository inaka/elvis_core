-module(forbidden_macro_names).

-define(FORBIDDEN,  "THIS IS FORBIDDEN").
-define(NOT_forbidden, "This is not FORBIDDEN since it's lowercase").
-define(ITS_FORBIDDEN(Arg), "This one is also forbidden").
-define (FORBIDDEN_TOO, also:forbidden).

-export([use/0]).

use() ->
  ?FORBIDDEN_TOO(?ITS_FORBIDDEN({?FORBIDDEN, ?NOT_forbidden})).
