# No Specs

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

`-spec` attributes **in header files** should be avoided.

> Works on `.beam` file? Yes, but it's not useful there. This rule is meant to be used for header files.

## Quick fix

Move the `-spec` next to the function it pertains to.

## Rationale

Placing `-spec` attributes in header files (`.hrl`) can lead to inconsistencies and maintenance
issues, especially when the same header is included in multiple modules. Specs belong with the
function implementation to ensure they stay in sync and reflect the function's actual interface.
Keeping `-specs` in implementation (`.erl`) files improves maintainability, avoids duplication,
and ensures tools like Dialyzer have accurate, module-specific context for analysis.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_specs, #{}}
```
