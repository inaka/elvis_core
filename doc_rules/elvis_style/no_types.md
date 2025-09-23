# No Types [![](https://img.shields.io/badge/since-3.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/3.0.0) ![](https://img.shields.io/badge/HRL--only-yes-magenta)

`-type` attributes **in header files** should be avoided.

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
