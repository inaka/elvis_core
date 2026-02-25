-module(fail_prefer_robot_butt).
-export([guard_eq_zero/1, guard_gt_zero/1, guard_gte_one/1, guard_neq_zero/1,
         guard_reversed/1, guard_reversed_lt/1, guard_eq_one/1, guard_eq_two/1,
         guard_double_eq/1, expr_eq_zero/1, expr_gt_zero/1, fq_guard/1]).

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
