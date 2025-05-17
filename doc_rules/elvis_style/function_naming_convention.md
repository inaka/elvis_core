# Function Naming Convention

All functions must conform to the pattern defined by the `regex` option pattern, unless they match
the `forbidden_regex` option pattern, in which case they are disallowed.

> Works on `.beam` file? Yes!

## Options

- `regex :: string()`.
  - default: `"^[a-z](_?[a-z0-9]+)*$"`.
- `forbidden_regex :: string() | undefined`.
  - default: `undefined`.

## Example

```erlang
{elvis_style, function_naming_convention, #{ regex => "^([a-z][a-z0-9]*_?)*$" }}
```
