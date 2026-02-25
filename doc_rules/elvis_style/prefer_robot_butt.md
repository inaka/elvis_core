# Prefer Robot Butt ![](https://img.shields.io/badge/BEAM-yes-orange)

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

% Exact length checks (only 1 and 2 are flagged)
guard_one(L) when length(L) =:= 1 -> one.
guard_two(L) when length(L) =:= 2 -> two.

% In expressions
expr_empty(L) -> length(L) =:= 0.
expr_nonempty(L) -> length(L) > 0.
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

Equality comparisons with integers greater than 2 (e.g. `length(L) =:= 10`) are not flagged, since
pattern matching on long list shapes is impractical.

## Options

- None.

## Example configuration

```erlang
{elvis_style, prefer_robot_butt, #{}}
```
