# Atom Naming Convention [![](https://img.shields.io/badge/since-1.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/1.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

All atoms should conform to the pattern defined by the `regex` option pattern, unless they match the
optional `forbidden_regex` pattern, in which case they are disallowed.

Atoms enclosed in apostrophes are treated differently due to their special syntax and are governed
by a separate configuration option. To apply the same pattern as `regex`, use the keyword `same`.

To disallow specific enclosed atoms (analogous to `forbidden_regex` for standard atoms),
use `forbidden_enclosed_regex`. Again, `same` may be used to reuse the forbidden_regex value.

## Avoid

> This is a convention aimed at ensuring consistency, rather than a coding issue.

```erlang
an____atom
```

## Prefer

```erlang
an_atom
```

## Rationale

By defining a regular expression for naming atoms you increase consistency across your codebase.

## Options

- `regex :: string()`
  - default: `"^[a-z](_?[a-z0-9]+)*(_SUITE)?$"`
- `enclosed_atoms :: string() | same`
  - default: `".*"`
- `forbidden_regex :: string() | undefined` [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0)
  - default: `undefined`
- `forbidden_enclosed_regex :: string() | undefined | same` [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0)
  - default: `undefined`

## Example configuration

```erlang
{elvis_style, atom_naming_convention, #{ regex => "^[a-z](_?[a-z0-9]+)*(_SUITE)?$"
                                       , enclosed_atoms => ".*"
                                       , forbidden_regex => undefined
                                       , forbidden_enclosed_regex => undefined
                                       }}
```
