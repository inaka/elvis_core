# Protocol for deps rebar

(not available since [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0))

`rebar.config` dependencies should use known protocols.

> [!WARNING]
> This rule is now deprecated, having been replaced by `protocol_for_deps`.

## Options

- `regex :: string()`
  - default: `(https://.*|[0-9]+([.][0-9]+)*)`

## Example

```erlang
{elvis_project, protocol_for_deps_rebar, #{ regex => "(https://.*|[0-9]+([.][0-9]+)*)" }}
```
