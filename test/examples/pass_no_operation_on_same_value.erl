-module(pass_no_operation_on_same_value).

-export([boolean_ops/2, meaningless_ops/1, without_variables/0, other_operators/0, operation_on_records/0]).

-record(a_record, {a_field}).

boolean_ops(A, B) ->
    #{
        'and' => A and B,
        'or' => A or B,
        'xor' => A xor B,
        '==' => A == B,
        '/=' => A /= B,
        '=<' => A =< B,
        '<' => A < B,
        '>=' => A >= B,
        '>' => A > B,
        '=:=' => A =:= B,
        '=/=' => A =/= B,
        'andalso' => A andalso B,
        'orelse' => A orelse B
    }.

meaningless_ops(A) ->
    {something, around, B} = {something, around, A},
    B -- A.

without_variables() ->
    [
        {things, are} /= {"not", not_literals},
        function:application() == function:application(),
        should() andalso should(),
        not fail() orelse (not fail())
    ].

should() -> true.
fail() -> false.

other_operators() ->
    % we should not report a warning on these
    A = 1,
    [
        A + A,
        A - A,
        -A,
        +A,
        A / A,
        A div A,
        A * A,
        [A] ++ [A]
    ].

operation_on_records() ->
    One = #a_record{a_field = 1},
    Two = #a_record{a_field = 2},
    One#a_record.a_field < Two#a_record.a_field.
