# Variable Naming Convention

All variables should be named according to the regular expression provided.

> Works on `.beam` file? Yes!

## Options

- `regex :: string()`.
  - default: `"^_?([A-Z][0-9a-zA-Z]*)$"`.

## Example

```erlang
{elvis_style, variable_naming_convention, #{ regex => "^_?([A-Z][0-9a-zA-Z]*)$" }}
```
