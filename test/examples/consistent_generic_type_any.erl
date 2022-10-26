-module(consistent_generic_type_any).

-export([simple_any/1, simple_combined/1, simple_when/1]).

% Type definitions when this is alone or combined
-type my_type() :: any().
-type combined() :: any() | my_type().

% Record definitions
-record(my_record, {c :: combined(), a :: any()}).

% Callback definitions (with or without when)
-callback my_callback(any()) -> any().
-callback my_callback_when(X) -> X when X :: any().


-spec simple_any(any()) -> ok.
simple_any(_Args) -> ok.

-spec simple_combined(combined()) -> ok.
simple_combined(_Args) -> ok.

-spec simple_when(#my_record{}) -> {combined(), X} when X :: any().
simple_when(#my_record{c = C, a = A}) -> {C, A}.
