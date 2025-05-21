# No Match in Condition

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

Pattern-matching in `case` expressions should be avoided.

> Works on `.beam` file? Yes!

## Avoid

```erlang
case #{a := A} = do:something() of
    #{b := good} -> {a, really, nice, A};
    #{b := bad} -> {"not", a, good, A}
end
```

## Prefer

Prefer pattern-matching in the clause heads:

```erlang
case do:something() of
    #{a := A, b := good} -> {a, really, nice, A};
    #{a := A, b := bad} -> {"not", a, good, A}
end
```

## Rationale

Pattern matching directly in the `case` expression can lead to fragile or misleading code,
especially when multiple branches depend on specific data structures or values. This approach
can obscure the control flow and increase the risk of runtime errors due to unmatched patterns.
Instead, perform pattern matching in case clauses.
This improves readability, debuggability, and the clarity of intent.

## Options

- None.

## Example

```erlang
{elvis_style, no_match_in_condition, #{}}
```
