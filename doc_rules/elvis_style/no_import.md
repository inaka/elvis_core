# No import

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

`import` attributes should not be used.
The benefits of not writing fully-qualified names for some functions are largely
outweighed by the complexity that this introduces when trying to debug the code.

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, no_import, #{}}
```
