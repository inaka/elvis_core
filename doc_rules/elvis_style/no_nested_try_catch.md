# No Nested try...catch Blocks

`try...catch` expressions shouldn't be nested. In case there is a piece of code that can raise
different exceptions, there should either be only one `try...catch` block with guards for the
different exceptions, or those exceptions should be handled somewhere else.

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, no_nested_try_catch}
%% or
{elvis_style, no_nested_try_catch, #{}}
```
