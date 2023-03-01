# Max Anonymous Function Arity

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

This specifies an upper bound on anonymous function arity.
This rule is similar to `max_function_arity`
but it applies to anonymous functions (i.e., `fun(…) -> … end`)
and it's usually stricter than its counterpart (it's default is `5` instead of `8`).

> Works on `.beam` file? Yes

## Options

- `max_arity :: non_neg_integer()`.
  - default: `5`.

## Example

```erlang
{elvis_style, max_anonymous_function_arity}
%% or
{elvis_style, max_anonymous_function_arity, #{max_arity => 10}}
```
