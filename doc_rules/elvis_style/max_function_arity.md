# Max Function Arity

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

This specifies an upper bound on function arity.
This rule is similar to `max_anonymous_function_arity`
but it applies to regular functions only (not anonymous ones).

> Works on `.beam` file? Yes

## Options

- `max_arity :: non_neg_integer()`.
  - default: `8`.
- `non_exported_max_arity :: non_neg_integer() | same`.
  - default: `8`.

## Example

```erlang
{elvis_style, max_function_arity}
%% or
{elvis_style, max_function_arity, #{max_arity => 10, non_exported_max_arity => 12}}
```
