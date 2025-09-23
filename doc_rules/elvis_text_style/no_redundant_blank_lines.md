# No Redundant Blank Lines [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0)

No more than `max_lines` consecutive blank lines are allowed.

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
