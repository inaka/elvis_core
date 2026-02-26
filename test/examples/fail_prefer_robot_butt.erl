-module(fail_prefer_robot_butt).

-feature(maybe_expr, enable).

-export([guard_eq_zero/1, guard_gt_zero/1, guard_gte_one/1, guard_neq_zero/1,
         guard_reversed/1, guard_reversed_lt/1, guard_eq_one/1, guard_eq_two/1,
         guard_double_eq/1, expr_eq_zero/1, expr_gt_zero/1, fq_guard/1,
         case_with_length/1, try_of_with_length/0, if_with_length/1,
         maybe_with_length/0, receive_with_length/0,
         complex_guard_eq/1, complex_guard_gt/1,
         is_empty/1, bad_stuff/1]).

% Guards
guard_eq_zero(L) when length(L) =:= 0 -> empty.
guard_gt_zero(L) when length(L) > 0 -> notempty.
guard_gte_one(L) when length(L) >= 1 -> notempty.
guard_neq_zero(L) when length(L) =/= 0 -> notempty.
guard_reversed(L) when 0 =:= length(L) -> empty.
guard_reversed_lt(L) when 0 < length(L) -> notempty.
guard_eq_one(L) when length(L) =:= 1 -> one.
guard_eq_two(L) when length(L) =:= 2 -> two.
guard_double_eq(L) when length(L) == 0 -> empty.

% In expressions
expr_eq_zero(L) -> length(L) =:= 0.
expr_gt_zero(L) -> length(L) > 0.

% Fully qualified
fq_guard(L) when erlang:length(L) =:= 0 -> empty.

% case: pattern with guard length(X) =:= 0
case_with_length(X) ->
    case X of
        X when length(X) =:= 0 -> empty;
        _ -> nonempty
    end.

% try...of with guard
try_of_with_length() ->
    try return_list() of X when length(X) =:= 0 -> empty
    catch _:_ -> other
    end.
return_list() -> [].

% if with length comparison
if_with_length(X) ->
    if
        length(X) =:= 0 -> empty;
        true -> nonempty
    end.

% maybe with length in else-clause guard (maybe has else, not of)
maybe_with_length() ->
    maybe return_list()
    else
        X when length(X) =:= 0 -> empty;
        _ -> nonempty
    end.

% receive with guard
receive_with_length() ->
    receive
        {bla, bla, X} when length(X) =:= 0 -> empty
    after
        0 -> timeout
    end.

% Complex guards: is_list(X) andalso length(X) =:= 0 / > 0
complex_guard_eq(X) when is_list(X) andalso length(X) =:= 0 -> empty.
complex_guard_gt(X) when is_list(X) andalso length(X) > 0 -> notempty.

% Body: plain and in orelse
is_empty(X) -> length(X) =:= 0.
bad_stuff(X) -> length(X) =:= 0 orelse hd(X).
