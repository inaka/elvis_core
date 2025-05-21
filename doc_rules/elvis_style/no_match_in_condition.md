# No Match in Condition [![](https://img.shields.io/badge/since-3.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/3.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Pattern-matching in `case` expressions should be avoided.

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

## Example configuration

```erlang
{elvis_style, no_match_in_condition, #{}}
```
