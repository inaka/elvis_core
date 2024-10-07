# Operator Spaces

There should be a space in the position (e.g., `right` or `left`) of the operators specified. The
operator can be any string.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected)

## Options

- `rules :: [{right | left, string()}]`
  - default:
    - before [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0): `[{right, ","},
    {right, "++"}, {left, "++"}]`
    - since [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0):

```erlang
          [{right, "++"}, {left, "++"}, {right, "="}, {left, "="}, {right, "+"}, {left, "+"},
           {right, "-"}, {left, "-"}, {right, "*"}, {left, "*"}, {right, "/"}, {left, "/"},
           {right, "=<"}, {left, "=<"}, {right, "<"}, {left, "<"}, {right, ">"}, {left, ">"},
           {right, ">="}, {left, ">="}, {right, "=="}, {left, "=="}, {right, "=:="}, {left, "=:="},
           {right, "/="}, {left, "/="}, {right, "=/="}, {left, "=/="}, {right, "--"}, {left, "--"},
           {right, "=>"}, {left, "=>"}, {right, ":="}, {left, ":="}, {right, "<-"}, {left, "<-"},
           {right, "<="}, {left, "<="}, {right, "||"}, {left, "||"}, {right, "|"}, {left, "|"},
           {right, "::"}, {left, "::"}, {right, "->"}, {left, "->"}, {right, ","}, {left, "!"},
           {right, "!"}]
```

## Example

```erlang
{elvis_style, operator_spaces, #{ rules => [{right, ","}
                                          , {right, "++"}
                                          , {left, "++"}
                                           ]
                                }}
```
