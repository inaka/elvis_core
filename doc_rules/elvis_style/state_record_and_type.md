# State Record And Type ![](https://img.shields.io/badge/BEAM-yes-orange)

Modules implementing the OTP behaviors listed below should define a `#state{}` record and a
corresponding state type (public - `type()` - or private - `opaque`).

- `gen_server`
- `gen_event` [![](https://img.shields.io/badge/since-0.7.0-blue)](https://github.com/inaka/elvis_core/releases/tag/0.7.0)
- `gen_fsm`
- `gen_statem` [![](https://img.shields.io/badge/since-0.7.0-blue)](https://github.com/inaka/elvis_core/releases/tag/0.7.0)
- `supervisor_bridge`

**Note**: if used together with `export_used_types`, the `state` record **should be** defined as a
private type (`opaque()`), and should be exported.

`opaque()` was added in [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0).

## Avoid

No type declared, no type exported:

```erlang
-behaviour(gen_server).

-export([init/1]).

-record(state, {cur_value :: undefined | term()}).

init({}) ->
    {ok, #state{cur_value = undefined}}.
```

## Prefer

Type declared, type exported (as private):

```erlang
-behaviour(gen_server).

-export([init/1]).

-record(state, {cur_value :: undefined | term()}).
-opaque state() :: #state{}.
-export_type([state/0]).

-spec init({}) -> {ok, state()}.
init({}) ->
    {ok, #state{cur_value = undefined}}.
```

## Rationale

Modules that implement core OTP behaviors such as `gen_server`, `gen_event`, ... manage internal
state that is passed between callbacks. To improve readability, consistency, and type safety
(especially when using tools like Dialyzer), these modules should explicitly define a `#state{}`
record to represent their internal state, and a corresponding `-type state()` or `-opaque state()`
to abstract or encapsulate its structure.

This way, the state structure is clearly defined and documented, and type specifications referring
to the state are consistent across callbacks.

## Options

- None.

## Example configuration

```erlang
{elvis_style, state_record_and_type, #{}}
```
