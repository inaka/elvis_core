-module(pass_private_data_types).

-if(?OTP_RELEASE >= 29).
-record #my_nat_rec{a :: integer(), b :: integer(), c :: integer()}.
-else.
-record(my_nat_rec, {a :: integer(), b :: integer(), c :: integer()}).
-endif.

-record(my_rec, {a :: integer(), b :: integer(), c :: integer()}).

-opaque my_rec() :: #my_rec{}.
-opaque my_nat_rec() :: #my_nat_rec{}.

-export_type([my_rec/0, my_nat_rec/0]).

-export([hello/0]).

-spec hello() -> ok.
hello() ->
    my_fun(#my_rec{a = 1, b = 2, c = 3}, #my_nat_rec{a = 1, b = 2, c = 3}).

-spec my_fun(my_rec(), my_nat_rec()) -> ok.
my_fun(_Rec, _NatRec) -> ok.
