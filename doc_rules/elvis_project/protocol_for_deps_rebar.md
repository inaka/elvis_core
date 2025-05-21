# Protocol for deps rebar [![](https://img.shields.io/badge/until-1.4.0-red)](https://github.com/inaka/elvis_core/releases/tag/1.4.0)

`rebar.config` dependencies should use known protocols.

> [!WARNING]
> This rule is now deprecated, having been replaced by `protocol_for_deps`.

## Options

- `regex :: string()`
  - default: `(https://.*|[0-9]+([.][0-9]+)*)`

## Example configuration

```erlang
{elvis_project, protocol_for_deps_rebar, #{ regex => "(https://.*|[0-9]+([.][0-9]+)*)" }}
```
