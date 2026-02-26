-module(pass_prefer_robot_butt).
-export([empty/1, notempty/1, two/1, len/1, add_len/1, big/1]).

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
