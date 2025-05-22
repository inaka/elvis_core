# No Single-Clause Case Statements [![](https://img.shields.io/badge/since-3.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/3.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Single-clause case statements should be avoided.

## Avoid

```erlang
case do:something() of
    {ok, Result} -> do:something("else")
end
```

## Prefer

```erlang
{ok, Result} = do:something(),
do:something("else")
```

## Rationale

Using a `case` expression with only one clause is unnecessary and reduces code clarity. It adds
syntactic overhead without providing meaningful branching logic. In such cases, a let-style
assignment or direct pattern matching is typically more appropriate and idiomatic. Removing
single-clause case statements also improves readability and simplifies the control flow.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_single_clause_case, #{}}
```
