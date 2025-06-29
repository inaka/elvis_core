# No Nested Header Files [![](https://img.shields.io/badge/since-4.1.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.1.0) ![](https://img.shields.io/badge/HRL--only-yes-magenta)

`-include` and `-include_lib` attributes **in header files** should be avoided.

## Quick fix

Move the `-include[_lib]` attributes to the modules that actually need them.

## Rationale

Nesting include files makes it harder for the reader of a module to understand the origin of macros,
records, or other attributes found in them. It also leads to the usage of macros like the one shown
below to avoid duplicate definitions if more than one header file includes the same (different) header
file, too.

```erlang
-ifndef(HEADER_FILE).
…
-endif.
```

The code becomes unnecessarily complex and harder to maintain.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_nested_hrls, #{}}
```
