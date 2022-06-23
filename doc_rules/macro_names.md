# Macro Names

Macro names should only contain upper-case alphanumeric characters.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected)

## Options

- `regex :: string()`. (since [1.0.0](https://github.com/inaka/elvis_core/releases/tag/1.0.0))
  - default: `"^([A-Z][A-Z_0-9]+)$"`.

## Example

```erlang
{elvis_style, macro_names}
%% or
{elvis_style, macro_names, #{ regex => "^([A-Z][A-Z_0-9]+)$" }}
```
