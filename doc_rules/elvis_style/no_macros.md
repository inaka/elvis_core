# No Macros [![](https://img.shields.io/badge/since-1.3.2-blue)](https://github.com/inaka/elvis_core/releases/tag/1.3.2)

Avoid using macros unless absolutely necessary.

## Exceptions

Predefined macros (`MODULE`, `FUNCTION_NAME`, ...) are an exception to this rule, and can be used.

## Rationale

Macros can lead to code that is harder to debug, maintain, and understand due to their compile-time
expansion. They obscure logic by replacing code with textual substitutions, making the behavior of
the program less predictable and harder to trace. It's generally better to use functions, which are
more transparent and maintainable, unless there is a strong performance or architectural reason to
use macros.

## Options

- `allow :: [atom()]`
  - default: `[]`

## Example configuration

```erlang
{elvis_style, no_macros, #{ allow => [] }}
```
