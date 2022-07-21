-module(fail_no_successive_maps).

-export([bad/0, good/0]).

bad() ->
    M = #{this => is}#{wrong => "and"},
    M2 = M#{this := is}#{wrong := "as well"},
    M2#{this := is}#{also => wrong}.

good() ->
    M = #{this => is, good => "and"},
    M2 = M#{this := is, good := #{as => well}},
    M2#{this := is, also => good}.
