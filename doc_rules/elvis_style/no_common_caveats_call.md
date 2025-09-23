# No Common Caveats [![](https://img.shields.io/badge/since-0.4.0-blue)](https://github.com/inaka/elvis_core/releases/tag/0.4.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Usage of functions that are known to be inefficient or ambiguous, when more efficient or
explicit alternatives are available, should be avoided.

This rule also follows the recommendations from the
[Erlang Efficiency Guide – Common Caveats](https://www.erlang.org/doc/system/commoncaveats.html),
and provides warnings when these suboptimal functions are used.

## Rationale

Some standard library functions, while valid, have performance drawbacks or implicit behavior that
can lead to subtle bugs or inefficiencies - especially in performance-critical code. For example,
omitting the `Timeout` argument in `gen_server:call/2` may lead to default behavior that's not
always appropriate. Replacing these calls with their more efficient or explicit counterparts
results in clearer, faster, and more maintainable code.

## Options

- `caveat_functions :: [{module(), function(), arity()} | {module(), function()}]`
  - default: `[{timer, send_after, 2}
             , {timer, send_after, 3}
             , {timer, send_interval, 2}
             , {timer, send_interval, 3}
             , {erlang, size, 1}
             , {gen_statem, call, 2}
             , {gen_server, call, 2}
             , {gen_event, call, 3}
             , {erlang, list_to_atom, 1}
             , {erlang, binary_to_atom, 1}
             , {erlang, binary_to_atom, 2}
              ]`

`{gen_statem, call, 2}`, `{gen_server, call, 2}`, `{gen_event, call, 3}`,
`{erlang, list_to_atom, 1}`, `{erlang, binary_to_atom, 1}`, and `{erlang, binary_to_atom, 2}`
were added in
[4.1.0](https://github.com/inaka/elvis_core/releases/tag/4.1.0).

## Example configuration

```erlang
{elvis_style, no_common_caveats_call, #{ caveat_functions => [{timer, send_after, 2}
                                                            , {timer, send_after, 3}
                                                            , {timer, send_interval, 2}
                                                            , {timer, send_interval, 3}
                                                            , {erlang, size, 1}
                                                            , {gen_statem, call, 2}
                                                            , {gen_server, call, 2}
                                                            , {gen_event, call, 3}
                                                            , {erlang, list_to_atom, 1}
                                                            , {erlang, binary_to_atom, 1}
                                                            , {erlang, binary_to_atom, 2}
                                                             ]
                                       }}
```
