# Module Naming Convention ![](https://img.shields.io/badge/BEAM-yes-orange)

All modules should conform to the pattern defined by the `regex` option pattern, unless they match
the `forbidden_regex` option pattern, in which case they are disallowed.

**Note**: to mitigate the risk of namespace collisions and to maintain naming consistency across
the ecosystem, it is recommended that all modules within a given package or codebase adopt the
same prefix, typically that of the name of the project.

## Avoid

> This is a convention aimed at ensuring consistency, rather than a coding issue.

```erlang
-module('mod#1').
```

## Prefer

```erlang
-module(mod_nr_1).
```

## Rationale

By defining a regular expression for naming modules you increase consistency across your codebase.

## Options

- `regex :: string()`
  - default: `"^[a-z](_?[a-z0-9]+)*(_SUITE)?$"`
- `forbidden_regex :: string() | undefined` [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0)
  - default: `undefined`

`regex` was `"^([a-z][a-z0-9]*_?)*(_SUITE)?$"` until [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0).

## Example configuration

```erlang
{elvis_style, module_naming_convention, #{ regex => "^[a-z](_?[a-z0-9]+)*(_SUITE)?$"
                                         , forbidden_regex => undefined }}
```
