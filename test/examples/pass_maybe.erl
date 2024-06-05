-module(pass_maybe).

-if(?OTP_RELEASE >= 25).

-feature(maybe_expr, enable).

-endif.

-export([sum_numbers/2]).


-if(?OTP_RELEASE >= 25).
sum_numbers(Number1, Number2) ->
    maybe
        ValidNumber1 ?= validate_number(Number1),
        ValidNumber2 ?= validate_number(Number2),
        ValidNumber1 + ValidNumber2
    else
        {error, invalid_number} ->
            {error, "One or both inputs are invalid numbers"}
    end.

validate_number(Number) when is_number(Number) ->
    Number;
validate_number(_) ->
    {error, invalid_number}.
-else.
sum_numbers(Number1, Number2) ->
    Number1 + Number2.
-endif.
