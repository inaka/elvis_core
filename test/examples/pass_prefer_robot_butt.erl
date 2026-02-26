-module(pass_prefer_robot_butt).
-export([empty/1, notempty/1, two/1, len/1, add_len/1, big/1,
         length_minus_one_eq_zero/1, length_minus_two_eq_zero/1,
         length_minus_one_eq_one/1, zero_eq_length_minus_two/1,
         one_eq_length_minus_one/1, length_minus_one_gt_zero/1,
         zero_lt_length_minus_one/1, length_minus_one_neq_zero/1]).

empty([]) -> true;
empty(_) -> false.
notempty([_|_]) -> true;
notempty(_) -> false.
two([_, _]) -> true;
two(_) -> false.
% length used in non-comparison context is fine
len(L) -> length(L).
add_len(L) -> length(L) + 1.
% length compared to large N (> 2) is fine
big(L) when length(L) =:= 10 -> big.

% Arithmetic on length (length(X) - N op M) is not flagged
length_minus_one_eq_zero(X) -> length(X) - 1 =:= 0.
length_minus_two_eq_zero(X) -> length(X) - 2 =:= 0.
length_minus_one_eq_one(X) -> length(X) - 1 =:= 1.
zero_eq_length_minus_two(X) -> 0 =:= length(X) - 2.
one_eq_length_minus_one(X) -> 1 =:= length(X) - 1.
length_minus_one_gt_zero(X) -> length(X) - 1 > 0.
zero_lt_length_minus_one(X) -> 0 < length(X) - 1.
length_minus_one_neq_zero(X) -> length(X) - 1 =/= 0.
