# No Common Caveats

(since [0.4.0](https://github.com/inaka/elvis_core/releases/tag/0.4.0))

The [Erlang Efficiency Guide](https://erlang.org/doc/efficiency_guide/commoncaveats.html) has a list
of "Common Caveats" suggesting more efficient alternatives to several common functions.  This rule
provides warnings if you call "inefficient" functions with entirely equivalent (efficient)
alternatives. It also warns you about functions with a more explicit interface (e.g. `gen_server:call/3`
instead of `gen_server:call/2`) so you're sure to not have forgotten it.

> Works on `.beam` file? Yes!

## Options

- `caveat_functions :: [{module(), function(), arity()} | {module(), function()}]`.
  - default: `[{timer, send_after, 2}
             , {timer, send_after, 3}
             , {timer, send_interval, 2}
             , {timer, send_interval, 3}
             , {erlang, size, 1}
             , {gen_statem, call, 2}
             , {gen_server, call, 2}
             , {gen_event, call, 3}
              ]`.

**Notice**: this rule is not enforced by default. Check the
[example `elvis.config` file](../../README.md#configuration) to see how you can enforce it.

## Example

```erlang
{elvis_style, no_common_caveats_call}
%% or
{elvis_style, no_common_caveats_call, #{ caveat_functions => [{timer, send_after, 2}
                                                            , {timer, send_after, 3}
                                                            , {timer, send_interval, 2}
                                                            , {timer, send_interval, 3}
                                                            , {erlang, size, 1}
                                                             ]
                                       }}
```
