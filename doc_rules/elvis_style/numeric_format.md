# Numeric Format

(since [1.3.0](https://github.com/inaka/elvis_core/releases/tag/1.3.0))

All numbers should be formatted according to the regular expression(s) provided.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected, since
numbers there are just numbers; they cannot be formatted)

## Rationale

> This is a convention aimed at ensuring consistency, rather than a coding issue.

By default, all number formats are valid in Erlang — but inconsistent use of underscores or base
notation can make numbers harder to read or visually misleading. For example, `1_23_456_7890` is
technically valid but non-standard and visually confusing. This rule allows teams to define
acceptable numeric formats (e.g., `1234567890` or `1_234_567_890`) using regular expressions,
and receive warnings when deviations occur.

## Options

- `regex :: string()`
  - default: `".*"`
- `int_regex :: string() | same`
  - default: `same`
  - `same` - use the generic regex
- `float_regex :: string() | same`
  - default: `same`
  - `same` - use the generic regex

## Example

```erlang
{elvis_style, numeric_format, #{ regex => ".*"
                               , int_regex => same
                               , float_regex => same
                               }}
```
