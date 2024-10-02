# No Trailing Whitespace

There should be no lines that end with whitespace characters.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected, since
there's no notion of "whitespace" in BEAM files)

## Options

- `ignore_empty_lines :: boolean().`
  - default: `false`

## Example

```erlang
{elvis_text_style, no_trailing_whitespace, #{ ignore_empty_lines => true }}
%% or
{elvis_text_style, no_trailing_whitespace, #{ ignore_empty_lines => false }}
%% or (equivalently to 'false')
{elvis_text_style, no_trailing_whitespace}
```
