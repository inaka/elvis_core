# No Operation on Same Value

Avoid applying infix operations on the same value at each side.
Since the result is already known, they are redundant.

Things like ... | ... can be written as
----------------|------------------
`A and A`       | `A`
`A or A`        | `A`
`A xor A`       | `false`
`A == A`        | `true`
`A /= A`        | `false`
`A =< A`        | `true`
`A < A`         | `false`
`A >= A`        | `true`
`A > A`         | `false`
`A =:= A`       | `true`
`A =/= A`       | `false`
`A andalso A`   | `A`
`A orelse A`    | `A`
`A -- A`        | `[]`

> Works on `.beam` file? Yes!

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

## Example

```erlang
{elvis_style, no_operation_on_same_value, #{}}
```
