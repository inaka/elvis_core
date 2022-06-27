# No debug call

Don't leave debugging function calls such as `io:format` or `ct:pal` in your source code.

> Works on `.beam` file? Yes!

## Options

- `debug_functions :: [{module(), function(), arity()} | {module(), function()}]`.
  - default: `[{ct, pal}, {ct, print}, {io, format, 1}, {io, format, 2}, {erlang, display, 1}]`
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
