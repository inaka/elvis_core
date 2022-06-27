# Protocol for deps

Use a specific protocol for `deps`.

This rule was called `protocol_for_deps_rebar` before
[2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0).

## Options

- `regex :: string()`.
  - default: `(https://.*|[0-9]+([.][0-9]+)*)`.

## Example

- before [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0)

```erlang
{elvis_project, protocol_for_deps_rebar, #{ regex => "(https://.*|[0-9]+([.][0-9]+)*)" }}
```

- since [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0)

```erlang
{elvis_project, protocol_for_deps, #{ regex => "(https://.*|[0-9]+([.][0-9]+)*)" }}
```
