# No Redundant Blank Lines

(since [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0))

No more than `max_lines` consecutive blank lines are allowed.

> Works on `.beam` file? No.

## Quick fix

Use an Erlang code formatter that enforces strict rules for blank lines.

## Options

- `max_lines :: integer()`
  - default: `1` - means that a maximum of 1 blank line is permitted; 2 (consecutive) or more will
  trigger a warning

## Example configuration

```erlang
{elvis_text_style, no_redundant_blank_lines, #{ max_lines => 1 }}
```
