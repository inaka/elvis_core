# Nesting Level

There shouldn't be any nested expressions that exceed the specified level.

> Works on `.beam` file? Yes!

## Options

- `level :: pos_integer()`
  - default: `4` (prior to [0.7.0](https://github.com/inaka/elvis_core/releases/tag/0.7.0) was `3`)

## Example

```erlang
{elvis_style, nesting_level, #{ level => 4 }}
```
