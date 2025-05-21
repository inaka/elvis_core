# No Tabs

Lines should not start with tab characters; spaces should be used for indentation.

> Works on `.beam` file? Not really! (it consumes results Ok, but they're irrelevant)

## Quick fix

Use an Erlang code formatter that enforces strict rules for tab vs. space indentation.

## Options

- None.

## Example

```erlang
{elvis_text_style, no_tabs, #{}}
```
