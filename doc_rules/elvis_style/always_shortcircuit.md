# Always Shortcircuit

(since [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0))

Use short-circuiting operators instead of the regular ones.

> Works on `.beam` file? Yes!

## Problematic code

```erlang
Expr1 or Expr2
Expr1 and Expr2
```

## Correct code

```erlang
Expr1 orelse Expr2
Expr1 andalso Expr2
```

## Rationale

Even though `orelse`/`andalso` are more verbose than `or`/`and`,
they implement the behaviour expected by most developers, which
is to only evaluate `Expr2` when `Expr1` is:

- `false`, for `orelse`
- `true`, for `andalso`

This also avoids:
- unnecessary computation
- potential errors when running the right-hand side of expressions with prior expectations
- right-associative -related issues (e.g. `is_integer(X) and X > 0` is not the same as
`is_integer(X) and (X > 0)`)

## Options

- None.

## Example

```erlang
{elvis_style, always_shortcircuit, #{}}
```
