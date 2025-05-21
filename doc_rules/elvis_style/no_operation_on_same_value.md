# No Operation on Same Value [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Applying infix operations on the same value on both sides should be avoided.

## Avoid/prefer

Expression to avoid | Preferred
--------------------|----------
`Expr and Expr`     | `Expr`
`Expr or Expr`      | `Expr`
`Expr xor Expr`     | `false`
`Expr == Expr`      | `true`
`Expr /= Expr`      | `false`
`Expr =< Expr`      | `true`
`Expr < Expr`       | `false`
`Expr >= Expr`      | `true`
`Expr > Expr`       | `false`
`Expr =:= Expr`     | `true`
`Expr =/= Expr`     | `false`
`Expr andalso Expr` | `Expr`
`Expr orelse Expr`  | `Expr`
`Expr = Expr`       | `Expr`
`Expr -- Expr`      | `[]`

## Rationale

Using the same value on both sides of an infix operation (e.g., `Expr == Expr`, `Expr - Expr`,
or `Expr andalso Expr`) results in outcomes that are constant or trivial by definition. These
expressions are redundant and may indicate overlooked logic errors or placeholder code that was
never revised. Removing or rewriting them improves code clarity and avoids misleading intent.

## Options

- `operations :: [atom()]`
  - default:
    - `'and'`
    - `'or'`
    - `'xor'`
    - `'=='`
    - `'/='`
    - `'=<'`
    - `'<'`
    - `'>='`
    - `'>'`
    - `'=:='`
    - `'=/='`
    - `'andalso'`
    - `'orelse'`
    - `'='`
    - `'--'`

## Example configuration

```erlang
{elvis_style, no_operation_on_same_value, #{ operations => ['and'
                                                          , 'or'
                                                          , 'xor'
                                                          , '=='
                                                          , '/='
                                                          , '=<'
                                                          , '<'
                                                          , '>='
                                                          , '>'
                                                          , '=:='
                                                          , '=/='
                                                          , 'andalso'
                                                          , 'orelse'
                                                          , '='
                                                          , '--'
                                                           ]
                                           }}
```
