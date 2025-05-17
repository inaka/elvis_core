# No Redundant Blank Lines

No more than `max_lines` consecutive blank lines are allowed.

> Works on `.beam` file? No.

## Quick fix

Use an Erlang code formatter that enforces strict rules for blank lines.

## Options

- `max_lines :: integet().`
  - default: 1 (meaning 2 consecutive blank lines are allowed, but not 3)

## Example

```erlang
{elvis_text_style, no_redundant_blank_lines, #{ max_lines => 5 }}
```
