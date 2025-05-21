# No Types

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

`-type` attributes **in header files** should be avoided.

> Works on `.beam` file? Yes, but it's not useful there. This rule is meant to be used for header files.

## Quick fix

Move the `-type` next to the module/function it pertains to.

## Rationale

Placing `-type` attributes in header files (`.hrl`) can lead to inconsistencies and maintenance
issues, particularly when the same header is included in multiple modules. Keeping `-type`
declarations within implementation (`.erl`) files improves maintainability by localizing type
definitions to where they are used and owned. When combined with `-export_type(_)`, this also
leverages module-based namespacing, making type usage more explicit and reducing the risk of name
clashes.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_types, #{}}
```
