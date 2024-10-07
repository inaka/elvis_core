<!-- markdownlint-disable MD033 -->
# No <code>&&nbsp;</code>

(since [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0))

Do not use <code>&&nbsp;</code>, use `$\s` instead.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected)

## Options

- None.

## Example

```erlang
{elvis_style, no_dollar_space, #{}}
```
