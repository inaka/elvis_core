# Module Naming Convention

All modules must conform to the pattern defined by the `regex` option pattern, unless they match
the `forbidden_regex` option pattern, in which case they are disallowed.

> Works on `.beam` file? Yes!

## Problematic code

> This is a convention aimed at ensuring consistency, rather than a coding issue.

```erlang
-module('mod#1').
```

## Correct code

```erlang
-module(mod_nr_1).
```

## Rationale

By defining a regular expression for naming modules you increase consistency across your codebase.

## Options

- `regex :: string()`
  - default: `"^[a-z](_?[a-z0-9]+)*(_SUITE)?$"`
- `forbidden_regex :: string() | undefined`
  - default: `undefined`

## Example

```erlang
{elvis_style, module_naming_convention, #{ regex => "^[a-z](_?[a-z0-9]+)*(_SUITE)?$"
                                         , forbidden_regex => undefined }}
```
