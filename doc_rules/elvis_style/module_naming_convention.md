# Module Naming Convention

All modules should be named according to the regular expression provided.

> Works on `.beam` file? Yes!

## Options

- `regex :: string()`.
  - default: `"^([a-z][a-z0-9]*_?)*(_SUITE)?$"`.

## Example

```erlang
{elvis_style, module_naming_convention, #{ regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$" }}
```
