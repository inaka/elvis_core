# Function Naming Convention

All functions should be named according to the provided regular expression,
except if they match with a defined `forbidden_regex`.

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
