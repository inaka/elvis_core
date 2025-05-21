# Operator Spaces

Spaces should exist in specified text positions.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected)

## Quick fix

Use an Erlang code formatter that enforces strict spacing around operators.

## Rationale

> This is a convention aimed at ensuring consistency, rather than a coding issue.

This rule ensures consistent spacing in specific parts of the code, enhancing readability and
maintaining coding style consistency. While space usage is often a matter of convention, enforcing
it in specified positions helps avoid visual clutter and ambiguity in code. It can be particularly
useful in cases where the readability of operators, delimiters, or certain expressions could be
compromised without adequate spacing.

## Options

- `rules :: [{right | left, string()}]`
  - default:
    - before [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0): `[{right, ","},
    {right, "++"}, {left, "++"}]`
    - since [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0):

```erlang
          [{left, "!"}, {right, "!"}, {left, "*"}, {right, "*"}
         , {left, "+"}, {right, "+"}, {left, "++"}, {right, "++"}
         , {left, "-"}, {right, "-"}, {left, "--"}, {right, "--"}
         , {left, "->"}, {right, "->"}, {left, "/"}, {right, "/"}
         , {left, "/="}, {right, "/="}, {left, "::"}, {right, "::"}
         , {left, ":="}, {right, ":="}, {left, "<"}, {right, "<"}
         , {left, "<-"}, {right, "<-"}, {left, "<="}, {right, "<="}
         , {left, "="}, {right, "="}, {left, "=/="}, {right, "=/="}
         , {left, "=:="}, {right, "=:="}, {left, "=<"}, {right, "=<"}
         , {left, "=="}, {right, "=="}, {left, "=>"}, {right, "=>"}
         , {left, ">"}, {right, ">"}, {left, ">="}, {right, ">="}
         , {left, "|"}, {right, "|"}, {left, "||"}, {right, "||"}
         , {right, ","}]
```

## Example

```erlang
{elvis_style, operator_spaces, #{ rules => [{left, "!"}, {right, "!"}, {left, "*"}, {right, "*"}
                                          , {left, "+"}, {right, "+"}, {left, "++"}, {right, "++"}
                                          , {left, "-"}, {right, "-"}, {left, "--"}, {right, "--"}
                                          , {left, "->"}, {right, "->"}, {left, "/"}, {right, "/"}
                                          , {left, "/="}, {right, "/="}, {left, "::"}, {right, "::"}
                                          , {left, ":="}, {right, ":="}, {left, "<"}, {right, "<"}
                                          , {left, "<-"}, {right, "<-"}, {left, "<="}, {right, "<="}
                                          , {left, "="}, {right, "="}, {left, "=/="}, {right, "=/="}
                                          , {left, "=:="}, {right, "=:="}, {left, "=<"}, {right, "=<"}
                                          , {left, "=="}, {right, "=="}, {left, "=>"}, {right, "=>"}
                                          , {left, ">"}, {right, ">"}, {left, ">="}, {right, ">="}
                                          , {left, "|"}, {right, "|"}, {left, "||"}, {right, "||"}
                                          , {right, ","}
                                           ]
                                }}
```
