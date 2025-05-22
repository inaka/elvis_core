# Operator Spaces

Spaces should exist in specified text positions.

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

`rules` was `[{right, ","}, {right, "++"}, {left, "++"}]` until [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0).

`{left, "!"}, {right, "!"}` was added in [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0)).

## Example configuration

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
