-module(pass_no_space_after_pound).

-format ignore.

-record(good, {record :: #good{}}).

-type good_record() :: #good{}.
-type good_map() :: #{good => map}.

-export([good_records/0, good_maps/0, ignored/0]).

-spec good_records() -> #good{} | good_record().
good_records() ->
    R = #good{record = #good{}},
    #good{record = R2} = R,
    R2#good{record = element(#good.record, R)}.

-spec good_maps() -> #{good => map} | good_map().
good_maps() ->
    M = #{good => #{good => map}},
    #{good := M2} = M,
    M2#{good := maps:get(good, M)}.

%% Spaces after # should be ignored in comments
-spec ignored() -> any() | {should, be, ignored, in, atoms, too, ' # '}.
ignored() ->
    {"They should also be ignored in:", "strings",
     <<"binaries">>, 'and', ["Characters: ", $# ],
     even_if_they_appear_in_a, #{map => ' # '}}.
