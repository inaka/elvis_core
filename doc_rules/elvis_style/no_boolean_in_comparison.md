# No Boolean In Comparison

Avoid comparing expressions to booleans. Prefer using boolean operators directly.

This is because these expressions evaluate to true or false,
so you could get the same result by using either the variable
directly or negating the variable.

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, no_boolean_in_comparison, #{}}
```
