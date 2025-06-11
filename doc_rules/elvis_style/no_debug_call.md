# No Debug Call ![](https://img.shields.io/badge/BEAM-yes-orange)

Use of functions that are intended primarily for debugging should be avoided.

## Rationale

Debug-specific functions - such as `io:format/2`, and `erlang:display/1`, are often used temporarily
during development or testing to trace execution or inspect values. However, they may be
inadvertently left behind in code that is later deployed. Their presence in production code can
lead to performance degradation, unwanted output, or inconsistent logging behavior.

**Note**: the specific functions to flag are configured via the `debug_functions` option.

## Options

- `debug_functions :: [{module(), function(), arity()} | {module(), function()}]`
  - default: `[{ct, pal}, {ct, print}, {io, format, 1}, {io, format, 2}, {erlang, display, 1},
    {io, put_chars, 1}, {io, put_chars, 2}]`

`{erlang, display, 1}` was added in [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0).

`{io, put_chars, 1}, {io, put_chars, 2}` was added in [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0).

## Example configuration

```erlang
{elvis_style, no_debug_call, #{ debug_functions => [{ct, pal}
                                                  , {ct, print}
                                                  , {io, format, 1}
                                                  , {io, format, 2}
                                                  , {erlang, display, 1}
                                                  , {io, put_chars, 1}
                                                  , {io, put_chars, 2}
                                                   ]
                              }}
```
