# No Common Caveats [![](https://img.shields.io/badge/since-0.4.0-blue)](https://github.com/inaka/elvis_core/releases/tag/0.4.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Usage of functions that are known to be inefficient or ambiguous, when more efficient or
explicit alternatives are available, should be avoided.

This rule also follows the recommendations from the
[Erlang Efficiency Guide – Common Caveats](https://www.erlang.org/doc/system/commoncaveats.html),
and warns when these suboptimal functions are used.

## Rationale

Some standard library functions, while valid, have performance drawbacks or implicit behavior that
can lead to subtle bugs or inefficiencies—especially in performance-critical code. Replacing them
with clearer or better-scaling alternatives tends to make systems easier to reason about and to tune.

## Rationale for default `caveat_functions`

The built-in list exists so new users get guardrails aligned with Erlang/OTP guidance; you can
always narrow or extend it in configuration.

- **`timer:send_after/2,3` and `timer:send_interval/2,3`** — Traffic for the `timer` module goes
  through a dedicated process; the efficiency guide recommends **`erlang:start_timer/3,4`** and
  **`erlang:send_after/3,4`** for better scalability and clearer semantics in many designs.
- **`erlang:size/1`** — Works on both tuples and binaries; **`tuple_size/1`** and **`byte_size/1`**
  make intent explicit and allow the compiler to optimize better.
- **`gen_server:call/2`, `gen_statem:call/2`, and `gen_event:call/3`** — The two-argument (or
  three-argument for `gen_event`) forms use a **default timeout of `infinity`**, so a stuck peer can
  block the caller indefinitely. Prefer the variants with an explicit timeout unless you truly need
  to wait forever.
- **`erlang:list_to_atom/1` and `erlang:binary_to_atom/1,2`** — Every new atom is interned for the
  life of the node; unbounded creation (e.g. from user input) can exhaust the atom table. Prefer
  **`list_to_existing_atom/1`**, **`binary_to_existing_atom/1,2`**, maps, or binaries when identifiers
  are dynamic.
- **`erlang:garbage_collect/0`** — Forces a full collection on the calling process and can inject
  noticeable latency spikes; the runtime usually schedules GC well on its own, so this call should be
  rare and deliberate ([added in 5.0.0](https://github.com/inaka/elvis_core/releases/tag/5.0.0)).

`{gen_statem, call, 2}`, `{gen_server, call, 2}`, `{gen_event, call, 3}`,
`{erlang, list_to_atom, 1}`, `{erlang, binary_to_atom, 1}`, and `{erlang, binary_to_atom, 2}`
were added in
[4.1.0](https://github.com/inaka/elvis_core/releases/tag/4.1.0).

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
             , {erlang, garbage_collect, 0}
              ]`

## Example configuration

```erlang
{elvis_style, no_common_caveats_call, #{
    caveat_functions => [
        {timer, send_after, 2},
        {timer, send_after, 3},
        {timer, send_interval, 2},
        {timer, send_interval, 3},
        {erlang, size, 1},
        {gen_statem, call, 2},
        {gen_server, call, 2},
        {gen_event, call, 3},
        {erlang, list_to_atom, 1},
        {erlang, binary_to_atom, 1},
        {erlang, binary_to_atom, 2}
    ]
}}
```
