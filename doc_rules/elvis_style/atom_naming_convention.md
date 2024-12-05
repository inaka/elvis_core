# Atom Naming Convention

(since [1.0.0](https://github.com/inaka/elvis_core/releases/tag/1.0.0))

All atoms should be named according to the regular expression provided.
Except if it matches with a defined `forbidden_regex`.
Atoms enclosed in apostrophes have special meaning and are thus handled
by a different configuration option (use `same` if you want the same value as `regex`).
To define forbidden enclosed atoms (like the ones in `forbidden_regex` apply for `regex`), use `forbidden_enclosed_regex`(use `same` if you want the same value as `forbidden_regex`).

> Works on `.beam` file? Yes!

## Options

- `regex :: string()`.
  - default: `"^([a-z][a-z0-9]*_?)*(_SUITE)?$"`.
- `enclosed_atoms :: string()`.
  - default: `".*"`.
- `forbidden_regex :: string() | undefined`.
  - default: `undefined`.
- `forbidden_enclosed_regex :: string() | undefined`.
  - default: `undefined`.

## Example

```erlang
{elvis_style, atom_naming_convention, #{ regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$"
                                       , enclosed_atoms => ".*"
                                       }}
```
