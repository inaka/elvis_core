# No Redundant Blank Lines

There should be no more blank lines after each other than the `max_lines` variable.

> Works on `.beam` file? No.

## Options

- `max_lines :: integet().`
  - default: 1 (1 means 2 blank lines after each other)

## Example

```erlang
{elvis_text_style, no_redundant_blank_lines, #{ max_lines => 5 }}
%% or
{elvis_text_style, no_redundant_blank_lines}
```
