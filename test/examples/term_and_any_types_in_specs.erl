-module(term_and_any_types_in_specs).

-export([simple_term/1, simple_any/1]).

-spec simple_term(term()) -> ok.
simple_term(_Args) -> ok.

-spec simple_any(any()) -> ok.
simple_any(_Args) -> ok.