-module(fail_no_successive_maps).

-export([bad/0, good/0]).

-if(?OTP_RELEASE<27).
bad() ->
    M = #{this => is}#{wrong => "and"},
    M2 = M#{this := is}#{wrong := "as well"},
    M2#{this := is}#{also => wrong}.
-else.
bad() ->
    #{}.
-endif.

-if(?OTP_RELEASE<27).
good() ->
    M = #{this => is, good => "and"},
    M2 = M#{this := is, good := #{as => well}},
    M2#{this := is, also => good}.
-else.
good() ->
    #{}.
-endif.
