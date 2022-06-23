# Numeric Format

(since [1.3.0](https://github.com/inaka/elvis_core/releases/tag/1.3.0))

All numbers should be formatted according to the regular expression(s) provided. There is a general
regular expression, and also one for integers and another one for floats.
By default, all numbers are valid. The goal of the rule is to allow developers to be warn on the
presence of numbers like `1_23_456_7890` and require them to be written either as `1_234_567_890` or
`1234567890`, instead.

> Works on `.beam` file? No. (numbers there are just numbers; they can't be formatted)

## Options

- `regex :: string()`.
  - default: `".*"`.
- `int_regex :: string() | same`.
  - default: `same`.
  - `same` means _use the generic regex_
- `float_regex :: string() | same`.
  - default: `same`.
  - `same` means _use the generic regex_

## Example

```erlang
{elvis_style, numeric_format, #{ regex => "^[^_]*$"  % Don't use underscores
                               , int_regex => ".*"   % Except for integers
                               , float_regex => same
                               }}
```
