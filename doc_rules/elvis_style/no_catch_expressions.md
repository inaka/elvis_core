# No catch expressions

(since [1.4.0](https://github.com/inaka/elvis_core/releases/tag/1.4.0))

Use of catch expressions is discouraged, for performance reasons (always generates a stacktrace) and
consumption reasons (hindered readability, as per
[Catch and Throw](https://www.erlang.org/doc/reference_manual/expressions.html#catch))

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, no_catch_expressions, #{}}
```
