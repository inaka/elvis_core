-module(consistent_generic_type_term_and_any).

-export([simple_term/1, simple_combined/1, simple_when/1]).

% Type definitions when this is alone or combined
-type my_type() :: term().
-type combined() :: any() | my_type().

% Record definitions
-record(my_record, {t :: term(), a :: any()}).

% Callback definitions (with or without when)
-callback my_callback(term()) -> any().
-callback my_callback_when(X) -> X when X :: term().


-spec simple_term(term()) -> ok.
simple_term(_Args) -> ok.

-spec simple_combined(combined()) -> ok.
simple_combined(_Args) -> ok.

% Specs with when
-spec simple_when(#my_record{}) -> {any(), X} when X :: term().
simple_when(#my_record{a = A, t = T}) -> {A, T}.
