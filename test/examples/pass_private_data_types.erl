-module(pass_private_data_types).
-hank ignore.
-record(my_rec, {a, b, c}).

-opaque my_rec() :: #my_rec{}.

-export_type([my_rec/0]).
