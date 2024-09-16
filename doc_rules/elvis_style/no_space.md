# No Space

(since [1.4.0](https://github.com/inaka/elvis_core/releases/tag/1.4.0))

There should be no space in the position (e.g., `right` or `left`) of the text specified. The text
can be any string.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected)

## Options

- `rules :: [{right | left, string()}].`
  - default: `[{right, "("}, {left, ")"}, {left, ","}, {right, "#"}, {right, "?"}]`

## Example

```erlang
{elvis_style, no_space, #{ rules => [{right, "("}
                                   , {left, ")"}
                                   , {left, ","}
                                    ]
                                }}
```
