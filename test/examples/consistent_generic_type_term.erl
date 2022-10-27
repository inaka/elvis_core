-module(consistent_generic_type_term).

-export([simple_term/1, simple_combined/1, simple_when/1]).

% Type definitions when this is alone or combined
-type my_type() :: term().
-type combined() :: term() | my_type().

% Record definitions
-record(my_record, {c :: combined(), t :: term()}).

% Callback definitions (with or without when),
-callback my_callback(term()) -> term().
-callback my_callback_when(X) -> X when X :: term().


-spec simple_term(term()) -> ok.
simple_term(_Args) -> ok.

-spec simple_combined(combined()) -> ok.
simple_combined(_Args) -> ok.

% Specs with when
-spec simple_when(#my_record{}) -> {combined(), X} when X :: term().
simple_when(#my_record{c = C, t = T}) -> {C, T}.
