-module(fail_no_operation_on_same_value).

-export([boolean_ops/1, meaningless_ops/1, without_variables/0]).

boolean_ops(A) ->
    #{
        'and' => A and A,
        'or' => A or A,
        'xor' => A xor A,
        '==' => A == A,
        '/=' => A /= A,
        '=<' => A =< A,
        '<' => A < A,
        '>=' => A >= A,
        '>' => A > A,
        '=:=' => A =:= A,
        '=/=' => A =/= A,
        'andalso' => A andalso A,
        'orelse' => A orelse A
    }.

meaningless_ops(A) ->
    {something, around, A} = {something, around, A},
    {
        A -- A,
        A ++ A
    }.

without_variables() ->
    [
        {things, are} /= {things, are},
        [literals] == [literals]
    ].
