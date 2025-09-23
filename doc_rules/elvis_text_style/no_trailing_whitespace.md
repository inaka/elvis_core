# No Trailing Whitespace

Lines should not end with whitespace characters.

## Exceptions

Space-only lines may be excluded from this constraint, with option `ignore_empty_lines`.

## Quick fix

Use an Erlang code formatter that disallows trailing whitespace at the end of lines.

## Options

- `ignore_empty_lines :: boolean()`
  - default: `false`

## Example configuration

```erlang
{elvis_text_style, no_trailing_whitespace, #{ ignore_empty_lines => false }}
```
