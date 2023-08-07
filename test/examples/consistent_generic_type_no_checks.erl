-module(consistent_generic_type_no_checks).

-export([any/0, term/0, my_function/1]).

% A parametric type called any
-type any(Thing) :: Thing.

% A parametric type called term
-type term(Thing) :: Thing.

% Record definitions with attributes named term/any
-record(my_record, {term :: term, any :: any}).


% A function called any
-spec any() -> term(any).
any() -> any.

% A function called term
-spec term() -> any(term).
term() -> term.


% A function that calls the function called any
-spec my_function(Thing :: #my_record{}) -> any | term.
my_function(#my_record{any = any}) -> any();
my_function(#my_record{term = term}) -> term().
