-module(pass_private_data_types).

-record(my_rec, {a :: integer(), b :: integer(), c :: integer()}).

-opaque my_rec() :: #my_rec{}.

-export_type([my_rec/0]).

-export([hello/0]).

-spec hello() -> ok.
hello() ->
    my_fun(#my_rec{a = 1, b = 2, c = 3}).

-spec my_fun(my_rec()) -> ok.
my_fun(_Rec) -> ok.

