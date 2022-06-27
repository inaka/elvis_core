# Function Naming Convention

All functions should be named according to the regular expression provided.

> Works on `.beam` file? Yes!

## Options

- `regex :: string()`.
  - default: `"^([a-z][a-z0-9]*_?)*(_SUITE)?$"`.

## Example

```erlang
{elvis_style, function_naming_convention, #{ regex => "^([a-z][a-z0-9]*_?)*$" }}
```
