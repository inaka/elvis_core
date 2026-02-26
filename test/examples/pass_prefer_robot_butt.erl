-module(pass_prefer_robot_butt).
-export([empty/1, notempty/1, two/1, len/1, add_len/1, big/1,
         length_minus_one_eq_zero/1, case_tuple_with_length/1, try_tuple_with_length/1]).

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

% Not captured by the rule: length(X) - 1 =:= 0 (length not directly compared to integer).
% Robot butt would suggest matching on [_] instead.
length_minus_one_eq_zero(X) ->
    length(X) - 1 =:= 0.

% Not captured: length(X) inside a tuple, compared via pattern match {_, _, 0}.
% Robot butt would suggest matching the list in the tuple directly.
case_tuple_with_length(X) ->
    case {one_thing, "another thing", length(X)} of
        {_, _, 0} -> empty;
        _ -> nonempty
    end.

% Not captured: same idea in try...of.
try_tuple_with_length(X) ->
    try {one_thing, "another thing", length(X)} of
        {_, _, 0} -> empty;
        _ -> nonempty
    catch
        _:_ -> error
    end.
