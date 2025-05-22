# No Call [![](https://img.shields.io/badge/since-0.4.0-blue)](https://github.com/inaka/elvis_core/releases/tag/0.4.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Avoid calls to specific functions.

## Rationale

Some functions may be discouraged due to deprecation, known performance issues, unsafe side
effects, or because better alternatives exist. This rule serves as a centralized and customizable
enforcement point, allowing developers to prevent the use of problematic or non-idiomatic functions
and maintain consistency across a codebase.

## Options

- `no_call_functions :: [{module() | '_', function() | '_', arity() | '_'} |
  {module() | '_', function() | '_'}]`
  - default: `[]`

`'_'` wildcards was added in [3.2.0](https://github.com/inaka/elvis_core/releases/tag/3.2.0).

## Example configuration

```erlang
{elvis_style, no_call, #{ no_call_functions => [] }}
```
