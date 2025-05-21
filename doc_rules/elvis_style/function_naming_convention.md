# Function Naming Convention [![](https://img.shields.io/badge/since-2.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/2.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

All functions should conform to the pattern defined by the `regex` option pattern, unless they match
the `forbidden_regex` option pattern, in which case they are disallowed.

## Avoid

> This is a convention aimed at ensuring consistency, rather than a coding issue.

```erlang
a____function() -> ok.

...

another___function() ->
    ...
    ok.
```

## Prefer

```erlang
a_function() -> ok.

...

another_function() ->
    ...
    ok.
```

## Rationale

By defining a regular expression for naming functions you increase consistency across your codebase.

## Options

- `regex :: string()`
  - default: `"^[a-z](_?[a-z0-9]+)*(_test_)?$"`
- `forbidden_regex :: string() | undefined`
  - default: `undefined`

## Example configuration

```erlang
{elvis_style, function_naming_convention, #{ regex => "^[a-z](_?[a-z0-9]+)*(_test_)?$"
                                           , forbidden_regex => undefined
                                           }}
```
