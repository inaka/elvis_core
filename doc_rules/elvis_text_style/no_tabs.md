# No Tabs

There should be no lines that contain tabs instead of spaces.

> Works on `.beam` file? Not really! (it consumes results Ok, but they're irrelevant)

## Quick fix

Use an Erlang code formatter.

## Options

- None.

## Example

```erlang
{elvis_text_style, no_tabs}
%% or
{elvis_text_style, no_tabs, #{}}
```
