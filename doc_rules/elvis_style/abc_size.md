# ABC Size

Functions should not have excessively high ABC size.

The ABC size metric measures function size as a vector of three components:

- **A (Assignments)**: Count of match (`=`) expressions
- **B (Branches)**: Count of function calls
- **C (Conditions)**: Count of branching clauses (beyond the first in `case`/`if`/`receive`/`try`/`maybe`),
  `andalso`/`orelse` operators, and comparison operators (`==`, `/=`, `=:=`, `=/=`, `<`, `>`, `=<`, `>=`)

The final score is the vector magnitude: `sqrt(A^2 + B^2 + C^2)`.

Anonymous functions are not counted as part of the enclosing function's ABC size.

## Avoid

```erlang
big_function(Input) ->
    X1 = lists:seq(1, Input),
    X2 = lists:map(fun(X) -> X + 1 end, X1),
    X3 = lists:filter(fun(X) -> X > 5 end, X2),
    X4 = lists:sum(X3),
    X5 = lists:reverse(X2),
    X6 = case X4 > 100 andalso X4 < 1000 of
        true ->
            io:format("range: ~p~n", [X4]),
            X4 * 2;
        false ->
            0
    end,
    {X5, X6}.
```

## Prefer

Break large functions into smaller ones with fewer assignments, calls, and conditions:

```erlang
process(Input) ->
    Numbers = transform(Input),
    format_result(Numbers).

transform(Input) ->
    X1 = lists:seq(1, Input),
    X2 = lists:map(fun(X) -> X + 1 end, X1),
    lists:filter(fun(X) -> X > 5 end, X2).

format_result(Numbers) ->
    Total = lists:sum(Numbers),
    scale(Total).
```

## Rationale

A high ABC size indicates a function that does too much: too many variable bindings,
too many function calls, or too many conditional paths. Splitting such functions into
smaller, more focused units improves readability and testability.

## Options

- `max_abc_size :: number()`
  - default: `30`

## Example configuration

```erlang
{elvis_style, abc_size, #{ max_abc_size => 30 }}
```
