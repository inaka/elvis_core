# No Debug Call ![](https://img.shields.io/badge/BEAM-yes-orange)

Use of functions that are intended primarily for debugging should be avoided.

## Rationale

Debug-oriented APIs are invaluable while developing, but they are easy to leave in paths that later
run in CI, libraries, or production. They often write to the console, bypass structured logging,
or enable heavy tracing—hurting performance, polluting output, and making behavior harder to control
than a proper `logger` configuration.

**Note**: the specific functions to flag are configured via the `debug_functions` option.

## Rationale for default `debug_functions`

Defaults favor “obvious debug helpers” and tracing hooks; adjust the list if your project uses some
of these intentionally in non-debug code.

- **`ct:pal/*` and `ct:print/*`** — Common Test helpers for interactive runs; they should not appear
  outside test code.
- **`erlang:display/1`** — Prints to standard output for ad hoc inspection; prefer logging or
  explicit test assertions.
- **`io:format/1,2` and `io:put_chars/1,2`** — Often used as quick prints; production diagnostics
  should go through **`logger`** (or your application’s logging facade) for levels, formatters, and
  sinks.
- **`dbg`**, **`dyntrace`**, and **`instrument`** (any function) — Low-level tracing and debugging
  facilities; leaving calls in normal code paths can enable expensive work or surprise operators.

`{erlang, display, 1}` was added in [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0).

`{io, put_chars, 1}, {io, put_chars, 2}` was added in [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0).

`{dbg, '_'}, {dyntrace, '_'}, {instrument, '_'}` was added in [4.1.0](https://github.com/inaka/elvis_core/releases/tag/4.1.0).

## Options

- `debug_functions :: [{module(), function(), arity()} | {module(), function()}]`
  - default: `[{ct, pal}, {ct, print}, {erlang, display, 1}, {io, format, 1}, {io, format, 2},
    {io, put_chars, 1}, {io, put_chars, 2}, {dbg, '_'}, {dyntrace, '_'}, {instrument, '_'}]`

## Example configuration

```erlang
{elvis_style, no_debug_call, #{
    debug_functions => [
        {ct, pal},
        {ct, print},
        {erlang, display, 1},
        {io, format, 1},
        {io, format, 2},
        {io, put_chars, 1},
        {io, put_chars, 2},
        {dbg, '_'},
        {dyntrace, '_'},
        {instrument, '_'}
    ]
}}
```
