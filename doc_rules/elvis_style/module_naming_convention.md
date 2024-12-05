# Module Naming Convention

All modules should be named according to the regular expression provided.
Except if it matches with a defined `forbidden_regex`.

> Works on `.beam` file? Yes!

## Options

- `regex :: string()`.
  - default: `"^[a-z](_?[a-z0-9]+)*(_SUITE)?$"`.
- `forbidden_regex :: string() | undefined`.
  - default: `undefined`.

## Example

```erlang
{elvis_style, module_naming_convention, #{ regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$" }}
```
