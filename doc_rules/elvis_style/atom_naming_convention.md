# Atom Naming Convention

(since [1.0.0](https://github.com/inaka/elvis_core/releases/tag/1.0.0))

All atoms should be named according to the regular expression provided. Atoms enclosed in
apostrophes have special meaning and are thus handled by a different configuration option (use
`same` if you want the same value as `regex`).

> Works on `.beam` file? Yes!

## Options

- `regex :: string()`.
  - default: `"^([a-z][a-z0-9]*_?)*(_SUITE)?$"`.
- `enclosed_atoms :: string()`.
  - default: `".*"`.

## Example

```erlang
{elvis_style, atom_naming_convention, #{ regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$"
                                       , enclosed_atoms => ".*"
                                       }}
```
