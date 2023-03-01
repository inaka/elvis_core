-module(pass_private_data_types2).
-hank ignore.
-record(my_rec, {a, b, c}).

-type my_rec() :: #my_rec{}.
-type my_tuple() :: {bitstring(), bitstring()}.
-type my_map() :: map().
