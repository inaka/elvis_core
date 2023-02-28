# Export Used Types

(since [2.1.0](https://github.com/inaka/elvis_core/releases/tag/2.1.0))

Exporting a function without exporting the types it depends on can result in
reimplementing the types in every module that uses those functions. To avoid
this, when a function is exported, its types should be too.

## Options

- None.

## Example

```erlang
{elvis_style, export_used_types}
```
