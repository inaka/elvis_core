-module(pass_strict_term_equivalence).

-export([guards/2, body/2]).

-record(a_record, {
    equals = 1 =:= 1.0,
    different = 1 =/= 1.0
    }).

guards(A, B) when A =:= B -> equals;
guards(A, B) when A =/= B -> different.

body(A, B) ->
    case A of
        as_an_atom -> #{"this is valid" => '==', "This /= is also valid" => '/='};
        B -> #a_record{equals = A =:= B};
        _ -> A =/= B orelse A =:= B
    end.
