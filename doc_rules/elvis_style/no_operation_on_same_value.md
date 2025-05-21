# No Operation on Same Value

(since [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0))

Applying infix operations on the same value on both sides should be avoided.

> Works on `.beam` file? Yes!

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
