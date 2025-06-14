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
          [{right, "++"}, {left, "++"}, {right, "="}, {left, "="}
         , {right, "+"}, {left, "+"}, {right, "-"}, {left, "-"}
         , {right, "*"}, {left, "*"}, {right, "/"}, {left, "/"}
         , {right, "=<"}, {left, "=<"}, {right, "<"}, {left, "<"}
         , {right, ">"}, {left, ">"}, {right, ">="}, {left, ">="}
         , {right, "=="}, {left, "=="}, {right, "=:="}, {left, "=:="}
         , {right, "/="}, {left, "/="}, {right, "=/="}, {left, "=/="}
         , {right, "--"}, {left, "--"}, {right, "=>"}, {left, "=>"}
         , {right, ":="}, {left, ":="}, {right, "<-"}, {left, "<-"}
         , {right, "<="}, {left, "<="}, {right, "||"}, {left, "||"}
         , {right, "|"}, {left, "|"}, {right, "::"}, {left, "::"}
         , {right, "->"}, {left, "->"}, {right, ","}, {right, "!"}
         , {left, "!"}, {right, "?="}, {left, "?="}, {right, ";"},
         , {right, "<:="}, {left, "<:="}, {right, "<:-"}, {left, "<:-"}
         , {right, "&&"}, {left, "&&"}
          ]
```

`rules` was `[{right, ","}, {right, "++"}, {left, "++"}]` until [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0).

`{right, "!"}, {left, "!"}` was added in [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0).

<!-- markdownlint-disable MD013 -->
`{right, "?="}, {left, "?="}, {right, ";"}, {right, "<:="}, {left, "<:="}, {right, "<:-"}, {left, "<:-"}, {right, "&&"}, {left, "&&"}` was added in [4.1.0](https://github.com/inaka/elvis_core/releases/tag/4.1.0).
<!-- markdownlint-enable MD013 -->

## Example configuration

```erlang
{elvis_style, operator_spaces, #{ rules => [{right, "++"}, {left, "++"}, {right, "="}, {left, "="}
                                          , {right, "+"}, {left, "+"}, {right, "-"}, {left, "-"}
                                          , {right, "*"}, {left, "*"}, {right, "/"}, {left, "/"}
                                          , {right, "=<"}, {left, "=<"}, {right, "<"}, {left, "<"}
                                          , {right, ">"}, {left, ">"}, {right, ">="}, {left, ">="}
                                          , {right, "=="}, {left, "=="}, {right, "=:="}, {left, "=:="}
                                          , {right, "/="}, {left, "/="}, {right, "=/="}, {left, "=/="}
                                          , {right, "--"}, {left, "--"}, {right, "=>"}, {left, "=>"}
                                          , {right, ":="}, {left, ":="}, {right, "<-"}, {left, "<-"}
                                          , {right, "<="}, {left, "<="}, {right, "||"}, {left, "||"}
                                          , {right, "|"}, {left, "|"}, {right, "::"}, {left, "::"}
                                          , {right, "->"}, {left, "->"}, {right, ","}, {right, "!"}
                                          , {left, "!"}, {right, "?="}, {left, "?="}, {right, ";"},
                                          , {right, "<:="}, {left, "<:="}, {right, "<:-"}, {left, "<:-"}
                                          , {right, "&&"}, {left, "&&"}
                                           ]
                                }}
```
