-module(pass_max_map_type_keys_elvis_attr).

-elvis([{elvis_style, max_map_type_keys, #{max_keys => 30}}]).

-export_type(
    [t1/0, t2/0, t3/0, t5/0, t25/0, t26/0, t_good/0, t_not_map/0, t_inf_map/0]
).

-type t1() :: #{one := field}.

-type t2() :: #{one := field, two := fields}.

-type t3() :: #{one := field, two := fields, three := fields}.

-type t5() :: #{
    f1 => optional_field,
    f2 => optional_field,
    f3 => optional_field,
    f4 => optional_field,
    f5 => field
}.

-type t25() :: #{
    f01 := field,
    f02 := field,
    f03 := field,
    f04 := field,
    f05 := field,
    f06 := field,
    f07 := field,
    f08 := field,
    f09 := field,
    f10 := field,
    f11 := field,
    f12 := field,
    f13 := field,
    f14 := field,
    f15 := field,
    f16 := field,
    f17 := field,
    f18 := field,
    f19 := field,
    f20 := field,
    f21 := field,
    f22 := field,
    f23 := field,
    f24 := field,
    f25 := field
}.

-type t26() :: #{
    f01 := field,
    f02 := field,
    f03 := field,
    f04 := field,
    f05 := field,
    f06 := field,
    f07 := field,
    f08 := field,
    f09 := field,
    f10 := field,
    f11 := field,
    f12 := field,
    f13 := field,
    f14 := field,
    f15 := field,
    f16 := field,
    f17 := field,
    f18 := field,
    f19 := field,
    f20 := field,
    f21 := field,
    f22 := field,
    f23 := field,
    f24 := field,
    f25 := field,
    f26 := field
}.

-type t_good() :: #{
    this => #{
        is => a,
        map => with:many(keys),
        but => it,
        is => embedded,
        in => #{
            a => larger,
            map => so,
            it := is,
            not_affected := by,
            the := rule,
            _ => _
        }
    }
}.

-type t_not_map() :: {this, is, not_a_map}.

-type t_inf_map() :: #{
    this := map,
    has := potentially,
    infinite => fields,
    therefore => it,
    is => not_affected,
    by => the_rule,
    _ => term()
}.
