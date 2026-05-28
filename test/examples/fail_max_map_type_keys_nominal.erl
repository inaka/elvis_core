-module(fail_max_map_type_keys_nominal).

-if(?OTP_RELEASE >= 28).

-export_type([
    n2/0,
    n5/0
]).

-nominal n2() :: #{one := field, two := fields}.

-nominal n5() :: #{
    f1 => optional_field,
    f2 => optional_field,
    f3 => optional_field,
    f4 => optional_field,
    f5 => field
}.

-endif.
