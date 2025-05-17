# No Redundant Blank Lines

No more than `max_lines` consecutive blank lines are allowed.

> Works on `.beam` file? No.

## Quick fix

Use an Erlang code formatter that enforces strict rules for blank lines.

## Options

- `max_lines :: integer()`
  - default: `1` - means that a maximum of 2 consecutive blank lines is permitted; 3 or more will
  trigger a warning

## Example

```erlang
{elvis_text_style, no_redundant_blank_lines, #{ max_lines => 5 }}
```
