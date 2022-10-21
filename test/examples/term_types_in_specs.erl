-module(term_types_in_specs).

-export([simple_term/1]).

-spec simple_term(term()) -> ok.
simple_term(_Args) -> ok.