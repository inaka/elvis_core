# Variable Naming Convention

All variables should be named according to the provided regular expression,
except if they match with a defined `forbidden_regex`.

> Works on `.beam` file? Yes!

## Options

- `regex :: string()`.
  - default: `"^_?([A-Z][0-9a-zA-Z]*)$"`.
- `forbidden_regex :: string() | undefined`.
  - default: `undefined`.

## Example

```erlang
{elvis_style, variable_naming_convention, #{ regex => "^_?([A-Z][0-9a-zA-Z]*)$" }}
```
