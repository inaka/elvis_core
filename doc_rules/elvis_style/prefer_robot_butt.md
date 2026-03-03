# Prefer Robot Butt [![](https://img.shields.io/badge/since-5.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/5.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Using `length/1` only to check if a list is empty, non-empty, or has a specific small size should be
avoided in favour of pattern matching.

## Avoid

```erlang
% Empty check
guard_empty(L) when 0 =:= length(L) -> empty.

% Non-empty check
guard_nonempty(L) when length(L) > 0 -> notempty.
guard_nonempty(L) when length(L) >= 1 -> notempty.
guard_nonempty(L) when length(L) =/= 0 -> notempty.

% Exact length checks (1 and 2 flagged by default; configurable up to max_small_list_size)
guard_one(L) when length(L) =:= 1 -> one.
guard_two(L) when length(L) =:= 2 -> two.

% In expressions
expr_empty(L) -> length(L) =:= 0.
expr_nonempty(L) -> length(L) > 0.

% case/try: tuple contains length/1 and a clause matches 0/1/2 at that position
case_tuple_with_length(X) ->
    case {one_thing, "another thing", length(X)} of
        {_, _, 0} -> empty;
        _ -> nonempty
    end.
```

## Prefer

```erlang
% Empty: match on []
empty([]) -> true;
empty(_) -> false.

% Non-empty: match on [_|_] (the "robot butt" pattern)
notempty([_|_]) -> true;
notempty(_) -> false.

% One element: match on [_]
one([_]) -> true;
one(_) -> false.

% Two elements: match on [_, _]
two([_, _]) -> true;
two(_) -> false.
```

## Rationale

`length/1` traverses the entire list to compute its length (O(n)). Using it only to check emptiness,
non-emptiness, or a small fixed size is wasteful when pattern matching achieves the same result in
O(1). The name "robot butt" refers to the `[_|_]` pattern, which is the idiomatic way to match
non-empty lists.

Equality comparisons with integers greater than `max_small_list_size` (e.g. `length(L) =:= 10` when
using the default) are not flagged, since pattern matching on long list shapes is impractical.
Arithmetic involving `length/1` (e.g. `length(L) - N =:= M`) is not flagged.

**Case/try with tuple:** If the expression is a tuple that contains a `length/1` call in some
position, and at least one clause pattern is a tuple with 0, 1, or 2 (within `max_small_list_size`)
at that same position, the rule suggests matching on the list directly (e.g. `[]` or `[_|_]`)
instead of building a tuple with `length/1` and matching on the integer.

## Options

- **`max_small_list_size`** (default: `2`) — Maximum list length for which equality comparisons
  are flagged. For example, with `max_small_list_size => 5`, `length(L) =:= 3`, `=:= 4`, and `=:= 5`
  are also reported, with suggestions to match on `[_, _, _]`, `[_, _, _, _]`, and `[_, _, _, _, _]`
  respectively.

## Example configuration

```erlang
% Default: only 0, 1, 2
{elvis_style, prefer_robot_butt, #{}}

% Flag equality checks up to length 5
{elvis_style, prefer_robot_butt, #{ max_small_list_size => 5 }}
```
