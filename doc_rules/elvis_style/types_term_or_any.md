# Types Term vs Any

Use `term()` or `any()` consistenttly for types in specs.

> Works on `.beam` file? Yes.

## Options

- `preference :: term | any`.
  - default: `no_preference`.

## Example

```erlang
{elvis_style, types_term_or_any, #{ preference => term }}
```
