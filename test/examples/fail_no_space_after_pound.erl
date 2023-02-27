-module(fail_no_space_after_pound).

-format ignore.

-record(bad, {record :: # bad{}}).

-type bad_record() :: # bad{}.
-type bad_map() :: # {bad => map}.

-export([bad_records/0, bad_maps/0]).

-spec bad_records() -> # bad{}.
bad_records() ->
    R = # bad{record = # bad{}},
    # bad{record = R2} = R,
    R2# bad{record = element(# bad.record, R)}.

-spec bad_maps() -> # {bad => map}.
bad_maps() ->
    M = # {bad => # {bad => map}},
    # {bad := M2} = M,
    M2# {bad := maps:get(bad, M)}.
