# Atom Naming Convention

(since [1.0.0](https://github.com/inaka/elvis_core/releases/tag/1.0.0))

All atoms must conform to the pattern defined by the `regex` option pattern, unless they match the
`forbidden_regex` option pattern, in which case they are disallowed.

Atoms enclosed in apostrophes are treated differently due to their special syntax and are governed
by a separate configuration option. To apply the same pattern as `regex`, use the keyword `same`.

To disallow specific enclosed atoms (analogous to `forbidden_regex` for standard atoms),
use `forbidden_enclosed_regex`. Again, same may be used to reuse the forbidden_regex value.

> Works on `.beam` file? Yes!

## Problematic code

> This is a convention aimed at ensuring consistency, rather than a coding issue.

```erlang
an____atom
```

## Correct code

```erlang
an_atom
```

## Rationale

By defining a regular expression for naming atoms you increase consistency across your codebase.

## Options

- `regex :: string()`
  - default: `"^([a-z][a-z0-9]*_?)*(_SUITE)?$"`
- `enclosed_atoms :: string() | same`
  - default: `".*"`
- `forbidden_regex :: string() | undefined`
  - default: `undefined`
- `forbidden_enclosed_regex :: string() | undefined | same`
  - default: `undefined`

## Example

```erlang
{elvis_style, atom_naming_convention, #{ regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$"
                                       , enclosed_atoms => ".*"
                                       , forbidden_regex => undefined
                                       , forbidden_enclosed_regex => undefined
                                       }}
```
