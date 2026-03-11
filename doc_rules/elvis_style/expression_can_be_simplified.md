# Expression Can Be Simplified [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Expressions that are mathematically or logically redundant (e.g. identity or absorbing constants)
should be simplified.

## Avoid/prefer

| Expression to avoid | Prefer   | Label (for `simplifications`) |
|---------------------|----------|-------------------------------|
| `[] ++ X`           | `X`      | `list_append_left_empty`      |
| `X ++ []`           | `X`      | `list_append_right_empty`     |
| `[] -- X`           | `[]`     | `list_subtract_from_empty`    |
| `X -- []`           | `X`      | `list_subtract_empty`         |
| `X + 0`             | `X`      | `add_zero_right`              |
| `X - 0`             | `X`      | `subtract_zero`               |
| `0 - X`             | `-X`     | `subtract_from_zero`          |
| `X * 1`             | `X`      | `multiply_by_one_right`       |
| `1 * X`             | `X`      | `multiply_by_one_left`        |
| `X div 1`           | `X`      | `div_by_one`                  |
| `X rem 1`           | `0`      | `rem_by_one`                  |
| `true andalso X`    | `X`      | `andalso_true`                |
| `false orelse X`    | `X`      | `orelse_false`                |
| `not true`          | `false`  | `not_true`                    |
| `not false`         | `true`   | `not_false`                   |
| `X band -1`         | `X`      | `band_neg_one`                |
| `X bor 0`           | `X`      | `bor_zero`                    |
| `X bxor 0`          | `X`      | `bxor_zero`                   |

## Rationale

Operations with identity or absorbing constants (e.g. `X + 0`, `[] ++ X`, `true andalso X`) have
well-defined outcomes that can be written more clearly by using the simplified form.
Simplifying improves readability and avoids redundant code.
This rule is separate from *No Operator With Same Values*,
which targets expressions like `X op X` where both operands are the same.

## Options

- `simplifications :: [atom()]`
  - List of simplification patterns to check. Omit a label to disable that pattern.
  - Default: all of the following
  - Labels: `list_append_left_empty`, `list_append_right_empty`, `list_subtract_from_empty`,
  `list_subtract_empty`, `add_zero_right`, `subtract_zero`, `subtract_from_zero`,
  `multiply_by_one_right`, `multiply_by_one_left`, `div_by_one`, `rem_by_one`, `andalso_true`,
  `orelse_false`, `not_true`, `not_false`, `band_neg_one`, `bor_zero`, `bxor_zero`

## Example configuration

```erlang
{elvis_style, expression_can_be_simplified, #{
    simplifications =>
        [
            list_append_left_empty,
            list_append_right_empty,
            list_subtract_from_empty,
            list_subtract_empty,
            add_zero_right,
            subtract_zero,
            subtract_from_zero,
            multiply_by_one_right,
            multiply_by_one_left,
            div_by_one,
            rem_by_one,
            andalso_true,
            orelse_false,
            not_true,
            not_false,
            band_neg_one,
            bor_zero,
            bxor_zero
        ]
    }
}
```

To enable only a subset (e.g. list and arithmetic simplifications):

```erlang
{elvis_style, expression_can_be_simplified, #{
    simplifications =>
        [
            list_append_left_empty,
            list_append_right_empty,
            list_subtract_empty,
            add_zero_right,
            subtract_zero,
            multiply_by_one_right,
            multiply_by_one_left
        ]
    }
}
```
