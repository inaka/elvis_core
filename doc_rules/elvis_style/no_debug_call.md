# No debug call

Don't leave debugging function calls, such as `io:format/1` or `ct:pal/1,2,3,4,5`, in your source
code.
The functions listed in option `debug_functions` are the ones you want the rule to warn you about.

> Works on `.beam` file? Yes!

## Options

- `debug_functions :: [{module(), function(), arity()} | {module(), function()}]`.
  - default: `[{ct, pal}, {ct, print}, {io, format, 1}, {io, format, 2}, {erlang, display, 1},
    {io, put_chars, 1}, {io, put_chars, 2}]`
  (`{erlang, display, 1}` is only included since
  [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0)).

## Example

```erlang
{elvis_style, no_debug_call}
%% or
{elvis_style, no_debug_call, #{ debug_functions => [{ct, pal}
                                                  , {ct, print}
                                                  , {io, format, 1}
                                                  , {io, format, 2}
                                                   ]
                              }}
```
