-module(fail_private_data_types).
-hank ignore.
-record(my_rec, {a, b, c}).

-type my_rec() :: #my_rec{}.
-type my_tuple() :: {bitstring(), bitstring()}.
-type my_map() :: map().

-export_type([my_rec/0]).
-export_type([my_tuple/0]).
-export_type([my_map/0]).
