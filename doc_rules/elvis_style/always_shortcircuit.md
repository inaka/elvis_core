# Always Shortcircuit

(since [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0))

`or` and `and` should be replaced with `orelse` and `andalso` to ensure short-circuit evaluation.

> Works on `.beam` file? Yes!

## Avoid

```erlang
Expr1 or Expr2

Expr1 and Expr2
```

## Prefer

```erlang
Expr1 orelse Expr2

Expr1 andalso Expr2
```

## Rationale

Although `orelse` and `andalso` are more verbose than `or` and `and`, they implement the behavior
expected by most developers, ensuring that `Expr2` is evaluated only when `Expr1` is:

- `false`, for `orelse`
- `true`, for `andalso`

This also prevents:

- unnecessary computations
- potential errors when running the right-hand side of expressions based on prior assumptions
- right-associativity issues (e.g. `is_integer(X) and X > 0` is not equivalent to
`is_integer(X) and (X > 0)`)

## Options

- None.

## Example

```erlang
{elvis_style, always_shortcircuit, #{}}
```
