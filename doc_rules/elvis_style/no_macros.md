# No Macros

(since [1.3.2](https://github.com/inaka/elvis_core/releases/tag/1.3.2))

Macros should be avoided. An exception to this is the usage of predefined macros, like `FUNCTION`,
and `MODULE`. User-defined exceptions can also be found in option `allow`.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected)

## Options

- `allow :: [atom()]`
  - default: `disable`.

## Example

```erlang
{elvis_style, no_macros}
%% or
{elvis_style, no_macros, #{ allow => ['SERVER'] }}
```
