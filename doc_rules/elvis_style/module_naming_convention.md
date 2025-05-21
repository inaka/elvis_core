# Module Naming Convention

All modules should conform to the pattern defined by the `regex` option pattern, unless they match
the `forbidden_regex` option pattern, in which case they are disallowed.

**Note**: to mitigate the risk of namespace collisions and to maintain naming consistency across
the ecosystem, it is recommended that all modules within a given package or codebase adopt the
same prefix, typically that of the name of the project.

> Works on `.beam` file? Yes!

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
- `forbidden_regex :: string() | undefined`
  - default: `undefined`

## Example configuration

```erlang
{elvis_style, module_naming_convention, #{ regex => "^[a-z](_?[a-z0-9]+)*(_SUITE)?$"
                                         , forbidden_regex => undefined }}
```
