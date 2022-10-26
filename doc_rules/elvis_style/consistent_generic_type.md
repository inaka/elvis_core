# Consistent Generic Type

Use `term()` or `any()` consistently for types in specs.

> Works on `.beam` file? Yes.

## Options

- `preferred_type :: term | any`.
- default: `term`.

## Example

```erlang
{elvis_style, consistent_generic_type, #{ preferred_type => term }}
```
