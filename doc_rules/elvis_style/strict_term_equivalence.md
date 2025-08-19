# Strict Term Equivalence [![](https://img.shields.io/badge/since-4.2.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.2.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Term [non-]equivalence (`=:=`, `=/=`) should be used instead of term [non-]equality (`==`, `/=`).

## Avoid

```erlang
Expr1 == Expr2 and Expr3 /= Expr4
```

## Prefer

```erlang
Expr1 =:= Expr2 and Expr3 =/= Expr4
```

## Rationale

`==` and `/=` perform numeric coercion, that may hide type errors or logic bugs.

`=:=` and `=/=` are safer (and more defensive) for logic, guards, and type-sensitive operations.

Using strict term equivalence also eliminates the need to have implicit assumptions about data types
and their representation, while encouraging explicit handling of mismatched types.

## Options

- None.

## Example configuration

```erlang
{elvis_style, strict_term_equality, #{}}
```
