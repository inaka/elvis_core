# Invalid Dynamic Calls

Avoid dynamic calls (i.e. `Module:f()`, `m:Function()` or `Module:Function()`) in modules where no
`callback` attribute is defined.

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, invalid_dynamic_call, #{}}
```
