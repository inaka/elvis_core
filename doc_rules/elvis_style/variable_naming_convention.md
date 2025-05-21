# Variable Naming Convention

All variable names should conform to the pattern defined by the `regex` option pattern, unless they
match the `forbidden_regex` option pattern, in which case they are disallowed.

> Works on `.beam` file? Yes!

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
- `forbidden_regex :: string() | undefined`
  - default: `undefined`

## Example

```erlang
{elvis_style, variable_naming_convention, #{ regex => "^_?([A-Z][0-9a-zA-Z]*)$"
                                           , forbidden_regex => undefined
                                           }}
```
