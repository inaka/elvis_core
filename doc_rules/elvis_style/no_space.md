# No Space [![](https://img.shields.io/badge/since-1.4.0-blue)](https://github.com/inaka/elvis_core/releases/tag/1.4.0)

Spaces in specified text positions should be avoided.

## Quick fix

Use an Erlang code formatter that enforces strict spacing.

## Avoid

```erlang
Text = ( value).
Label = # {key => err}.
```

## Prefer

```erlang
Text = (value).
Label = #{key => err}.
```

## Rationale

This rule improves code consistency and formatting by ensuring that specified positions - such as
`left`, or `right` - of a text string are free from extraneous spaces. Stray leading or trailing
whitespace may go unnoticed but can affect string comparisons, display output, or formatting in
generated documentation. Enforcing this rule helps prevent subtle issues and maintains a clean
codebase.

## Options

- `rules :: [{right | left, string()}]`
  - default: `[{right, "("}, {left, ")"}, {left, ","}, {left, ":"}, {right, "#"}, {right, "?"},
  {right, "?"}]`

`{right, "#"}, {right, "?"}` were added in [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0)).

## Example configuration

```erlang
{elvis_style, no_space, #{ rules => [{right, "("}
                                   , {left, ")"}
                                   , {left, ","}
                                   , {left, ":"}
                                   , {right, "#"}
                                   , {right, "?"}
                                   , {right, "?"}
                                    ]
                                }}
```
