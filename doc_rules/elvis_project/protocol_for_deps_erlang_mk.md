# Protocol for deps erlang.mk

(not available since [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0))

Use a specific protocol for `deps`.

## Options

- `regex :: string()`.
  - default: `(https://.*|[0-9]+([.][0-9]+)*)`.

## Example

```erlang
{elvis_project, protocol_for_deps_erlang_mk, #{ regex => "(https://.*|[0-9]+([.][0-9]+)*)" }}
```
