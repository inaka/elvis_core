# No throw

(since [1.4.0](https://github.com/inaka/elvis_core/releases/tag/1.4.0))

Raising exceptions with `throw` is discouraged. This rule warns you about unintended use of that
function.

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, no_throw, #{}}
```
