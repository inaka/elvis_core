# Variable Naming Convention ![](https://img.shields.io/badge/BEAM-yes-orange)

All variable names should conform to the pattern defined by the `regex` option pattern, unless they
match the `forbidden_regex` option pattern, in which case they are disallowed.

## Avoid

> This is a convention aimed at ensuring consistency, rather than a coding issue.

```erlang
My____var = 123.
```

## Prefer

```erlang
MyVar = 123.
```

## Rationale

By defining a regular expression for naming variables you increase consistency across your codebase.

## Options

- `regex :: string()`
  - default: `"^_?([A-Z][0-9a-zA-Z]*)$"`
- `forbidden_regex :: string() | undefined` [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0)
  - default: `undefined`

## Example configuration

```erlang
{elvis_style, variable_naming_convention, #{ regex => "^_?([A-Z][0-9a-zA-Z]*)$"
                                           , forbidden_regex => undefined
                                           }}
```
